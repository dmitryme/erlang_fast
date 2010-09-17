-module(erlang_fast_segment).

-export(
   [
      decode/2
      ,decode_template_id/2
      ,decode_pmap/2
      ,decode_fields/2
   ]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

decode(Data, Context) ->
   F = fun() ->
         {Data1, Context1} = decode_pmap(Data, Context),
         {Data2, Context2} = decode_template_id(Data1, Context1),
         decode_fields(Data2, Context2)
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
   case erlang_fast_decode_types:decode_uint(Data, false) of
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
   {DecodedField, Data1, Context1} = decode_field(
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

decode_field(Data, Instr, Context = #context{pmap = <<>>}) ->
   decode_field(Data, Instr, Context#context{pmap = <<0:1>>});

decode_field(Data, Instr, Context) when is_record(Instr, string) ->
   erlang_fast_string:decode(Data, Instr, Context);

decode_field(Data, Instr, Context)
when is_record(Instr, uInt32) or is_record(Instr, int32) or is_record(Instr, uInt64) or is_record(Instr, int64) ->
   erlang_fast_number:decode(Data, Instr, Context);

decode_field(Data, Instr, Context)
when is_record(Instr, decimal) ->
   erlang_fast_decimal:decode(Data, Instr, Context);

decode_field(Data, Instr = #sequence{}, Context) ->
   erlang_fast_sequence:decode(Data, Instr, Context);

decode_field(Data, Instr = #group{}, Context) ->
   erlang_fast_group:decode(Data, Instr, Context);

% dynamic templateRef
decode_field(Data, #templateRef{name = undef}, Context) ->
   {Msg, Data1, #context{dicts = D}} = decode(Data, Context),
   {Msg, Data1, Context#context{dicts = D}};

%static templateRef
decode_field(Data, #templateRef{name = Name}, Context = #context{template = T = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(Name, Context#context.templates#templates.tlist),
   {skip, Data, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

decode_field(Data, #typeRef{name = TypeName}, Context) ->
   {skip, Data, Context#context{application = TypeName}};

decode_field(_Data, Instr, _Context)  ->
   throw({error, [unknown_instruction, Instr]}).

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

create_fake_context(TemplateFileName) ->
   F = fun([], _) -> ok;
      (Err, Val) -> io:format("~p: ~p~n", [Err, Val])
   end,
   erlang_fast:create_context(TemplateFileName, F).

decode_test() ->
   Context = create_fake_context("doc/templates.xml"),
   Data = <<
   16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#b3, 16#09, 16#4a,
   16#6c, 16#e9, 16#83, 16#ae, 16#82, 16#1c, 16#4e, 16#0e, 16#80, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#80,
   16#06, 16#47, 16#a1, 16#01, 16#bd, 16#9e, 16#81, 16#82, 16#79, 16#41, 16#91, 16#b9, 16#84, 16#b0, 16#81, 16#b1,
   16#06, 16#3f, 16#a1, 16#7e, 16#d2, 16#f0, 16#80, 16#01, 16#39, 16#14, 16#c3, 16#23, 16#5a, 16#2f, 16#5f, 16#2d,
   16#31, 16#42, 16#b4, 16#09, 16#4a, 16#6c, 16#e9, 16#81, 16#b1, 16#81, 16#b0, 16#81, 16#7a, 16#0c, 16#a6, 16#97,
   16#fa, 16#c0, 16#d4, 16#01, 16#39, 16#14, 16#c4, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09,
   16#4a, 16#6c, 16#e9, 16#81, 16#1e, 16#b0, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#84, 16#81, 16#00, 16#53,
   16#f9, 16#86, 16#83, 16#fe, 16#03, 16#2e, 16#90, 16#1c, 16#4e, 16#0e, 16#80, 16#83, 16#c0, 16#ed, 16#01, 16#39,
   16#14, 16#c5, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09, 16#4a, 16#6c, 16#e9, 16#82, 16#2d>>,
   Data1 = <<>>,
   case decode(Data, Context) of
      not_enough_data ->
         ?debugFmt("not_enough_data", []),
         case decode(<<Data/binary, Data1/binary>>, Context) of
            not_enough_data ->
               error;
            {Msg, Data2, Context2} ->
               ?debugFmt("~p~n", [Msg])
         end;
      {Msg, Data2, Context1} ->
         ?debugFmt("~p~n", [Msg])
   end.
-endif.
