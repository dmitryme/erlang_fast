-module(erlang_fast_segment).

-export(
   [
      decode/2
      ,decode_template_id/2
      ,decode_pmap/2
      ,decode_fields/2
      ,encode/3
      ,encode_fields/2
   ]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_type/3
   ]).

-import(erlang_fast_encode_types,
   [
      encode_pmap/1,
      encode_type/3
   ]).

%% =========================================================================================================
%% decoding
%% =========================================================================================================

decode(Data, Context) ->
   try
      {Data1, Context1} = decode_pmap(Data, Context),
      {Data2, Context2} = decode_template_id(Data1, Context1),
      {Msg, Data3, Context3} = decode_fields(Data2, Context2),
      {Context3#context.template#template.name, Msg, Data3, Context3}
   catch
     _:Err ->
        Err
   end.

decode_template_id(Data, Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bits>>}) -> %
   case erlang_fast_dicts:get_value(global, ?common_template_id_key, Dicts) of
      undef ->
         throw({error, {'ERR D5', unable_to_know_template_id}});
      empty ->
         throw({error, {'ERR D6', unable_to_know_template_id}});
      Tid ->
         Template = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         {Data, Context#context{pmap = PMapRest, template = Template}}
   end;

decode_template_id(Data,
   Context = #context{dicts = Dicts, pmap = <<1:1, PMapRest/bits>>, logger = L}) -> % tid is present into stream
   case decode_type(uInt32, Data, false) of
      {Tid, Err, Data1} ->
         L(Err, Tid),
         Template = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         Dicts1 = erlang_fast_dicts:put_value(global, ?common_template_id_key, Tid, Dicts),
         {Data1, Context#context{pmap = PMapRest, template = Template, dicts = Dicts1}}
   end.

decode_pmap(Data, Context = #context{logger = L}) ->
   case erlang_fast_decode_types:decode_pmap(Data) of
      {Value, Err, Data1} ->
         L(Err, Value),
         {Data1, Context#context{pmap = Value}}
   end.

decode_fields(Data, Context = #context{template = #template{instructions = []}}) ->
   {[], Data, Context};

decode_fields(Data, Context = #context{template = Template = #template{instructions = [Instr | Tail]}}) ->
   {DecodedField, Data1, Context1} = erlang_fast_field_decode:decode(
      Data,
      Instr,
      Context#context{template = Template#template{instructions = Tail}}),
   {DecodedFields, Data2, Context2} = decode_fields(Data1, Context1),
   case DecodedField of
      skip ->
         {DecodedFields, Data2, Context2};
      {_FieldName, absent} ->
         {DecodedFields, Data2, Context2};
      DecodedField when is_list(DecodedField) ->
         {DecodedField ++ DecodedFields, Data2, Context2};
      DecodedField ->
         {[DecodedField | DecodedFields], Data2, Context2}
   end.

%% =========================================================================================================
%% encoding
%% =========================================================================================================

encode(TemplateId, MsgFields, Context) ->
   try
      Template = erlang_fast_templates:get_by_id(TemplateId, Context#context.templates#templates.tlist),
      {TidBin, Context1} = encode_template_id(TemplateId, Context#context{pmap = <<>>, template = Template}),
      {Data, _, Context2 = #context{pmap = PMap}} = encode_fields(MsgFields, Context1),
      {ok, {<<(encode_pmap(PMap))/bits, TidBin/bits, Data/bits>>, Context2}}
   catch
     _:Err ->
        Err
   end.

encode_template_id(Tid, Context = #context{pmap = PMap, dicts = Dicts, options = Options}) ->
   case proplists:get_bool(force_encode_tid, Options) of
      true ->
         {encode_type(uInt32, Tid, false), Context#context{pmap = <<PMap/bits, 1:1>>}};
      false ->
         case erlang_fast_dicts:get_value(global, ?common_template_id_key, Dicts) of
            DictValue when (DictValue == undef) orelse (DictValue =/= Tid) ->
               Dicts1 = erlang_fast_dicts:put_value(global, ?common_template_id_key, Tid, Dicts),
               {encode_type(uInt32, Tid, false), Context#context{pmap = <<PMap/bits, 1:1>>, dicts = Dicts1}};
            Tid ->
               {<<>>, Context#context{pmap = <<PMap/bits, 0:1>>}}
         end
   end.

encode_fields([], Context = #context{template = #template{instructions = []}}) ->
   {<<>>, [], Context};
encode_fields(MsgFields, #context{template = #template{instructions = []}})
  when is_list(MsgFields) andalso length(MsgFields) > 0 ->
   throw({error, {MsgFields, "not all message fields are encoded"}});
encode_fields(MsgFields, Context = #context{template = T = #template{instructions = [Instr | Rest]}}) ->
   {Head, MsgFields1, Context1} = erlang_fast_field_encode:encode(MsgFields, Instr, Context),
   {Tail, MsgFields2, Context2} = encode_fields(MsgFields1, Context1#context{template = T#template{instructions = Rest}}),
   {<<Head/bitstring, Tail/bitstring>>, MsgFields2, Context2}.
