-module(erlang_fast_segment).

-export(
   [
      decode/2
      ,decode_template_id/2
      ,decode_pmap/2
      ,decode_fields/2
      ,encode/2
   ]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_type/3
   ]).

%% =========================================================================================================
%% decoding
%% =========================================================================================================

decode(Data, Context) ->
   F =
   fun() ->
      {Data1, Context1} = decode_pmap(Data, Context),
      {Data2, Context2} = decode_template_id(Data1, Context1),
      {Msg, Data3, Context3 = #context{template = #template{id = TID}}} = decode_fields(Data2, Context2),
      {{TID, Msg}, Data3, Context3}
   end,
   F().
   %try F()
   %catch
   %  _:Err ->
   %     Err
   %end.

decode_template_id(Data, Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bitstring>>, logger = L}) -> %
   case erlang_fast_dicts:get_value(global, ?common_template_id_key, Dicts) of
      undef ->
         L('ERR D5', "Unable to get template ID from dictionary."),
         throw({'ERR D5', unable_to_know_template_id});
      empty ->
         L('ERR D6', "Template ID is empty in global dictionary."),
         throw({'ERR D6', unable_to_know_template_id});
      Tid ->
         Template = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         {Data, Context#context{pmap = PMapRest, template = Template}}
   end;

decode_template_id(Data,
   Context = #context{dicts = Dicts, pmap = <<1:1, PMapRest/bitstring>>, logger = L}) -> % tid is present into stream
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

encode({TID, MsgFields}, Context) ->
   F =
   fun() ->
      Template = erlang_fast_templates:get_by_id(TID, Context#context.templates#templates.tlist),
      {Data, _, Context1} = encode_fields(MsgFields, Context#context{template = Template}),
      {Data, Context1}
   end,
   try F()
   catch
     _:Err ->
        Err
   end.

encode_fields([], Context = #context{template = #template{instructions = []}}) ->
   {<<>>, [], Context};
encode_fields(MsgFields, Context = #context{template = #template{instructions = []}})
   when is_list(MsgFields) andalso length(MsgFields) > 0 ->
      {<<>>, MsgFields, Context};
%encode_fields(MsgFields, #context{template = #template{instructions = []}})
%   when is_list(MsgFields) andalso length(MsgFields) > 0 ->
%     throw({error, [MsgFields, "not all message fields are encoded"]});
encode_fields(MsgFields, Context = #context{template = T = #template{instructions = [Instr | Rest]}}) ->
   {Head, MsgFields1, Context1} = encode_field(MsgFields, Instr, Context),
   {Tail, MsgFields2, Context2} = encode_fields(MsgFields1, Context1#context{template = T#template{instructions = Rest}}),
   {<<Head/bitstring, Tail/bitstring>>, MsgFields2, Context2}.

encode_field(Instr = #field{type = string}, MsgFields, Context) ->
   erlang_fast_string:encode(MsgFields, Instr, Context);
%encode_field(_, Instr, _) ->
%   throw({error, [unknown_instruction, Instr]}).
encode_field(MsgFields, _Instr, Context) ->
   {<<>>, MsgFields, Context}.
