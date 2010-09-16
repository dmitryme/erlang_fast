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

-include_lib("eunit/include/eunit.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

decode(Data, Context) ->
   F = fun() ->
      {Context1, Data1} = decode_pmap(Data, Context),
      {Context2, Data2} = decode_template_id(Data1, Context1),
      decode_fields(Data2, Context2)
   end,
   F().
   %try F()
   %catch
   %  _:Err ->
   %     Err
   %end.

decode_template_id(Data, Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bitstring>>, logger = L}) -> %
   Tid = erlang_fast_dicts:get_value(global, ?common_template_id_key, Dicts),
   case Tid of
      undef ->
         L('ERR D5', "Unable to get template ID from dictionary."),
         throw({'ERR D5', unable_to_know_template_id});
      empty ->
         L('ERR D6', "Template ID is empty in global dictionary."),
         throw({'ERR D6', unable_to_know_template_id});
      Tid ->
         Template = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         {Context#context{pmap = PMapRest, template = Template}, Data}
   end;

decode_template_id(Data,
   Context = #context{dicts = Dicts, pmap = <<1:1, PMapRest/bitstring>>, logger = L}) -> % tid is present into stream
   Res = erlang_fast_decode_types:decode_uint(Data, false),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Tid, Err, Data1} ->
         L(Err, Tid),
         Template = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         Dicts1 = erlang_fast_dicts:put_value(global, ?common_template_id_key, Tid, Dicts),
         {Context#context{pmap = PMapRest, template = Template, dicts = Dicts1}, Data1}
   end.

decode_pmap(Data, Context = #context{logger = L}) ->
   Res = erlang_fast_decode_types:decode_pmap(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Value, Err, Data1} ->
         L(Err, Value),
         {Context#context{pmap = Value}, Data1}
   end.

decode_fields(Data, Context = #context{template = #template{instructions = []}}) ->
  {[], Context, Data};

decode_fields(Data, Context = #context{template = Template = #template{instructions = [Instr | Tail]}}) ->
   {DecodedField, Context1, Data1} = decode_field(
      Data,
      Instr,
      Context#context{template = Template#template{instructions = Tail}}),
   {DecodedFields, Context2, Data2} = decode_fields(Data1, Context1),
   case DecodedField of
      skip ->
         {DecodedFields, Context2, Data2};
      {_FieldName, absent} ->
         {DecodedFields, Context2, Data2};
      DecodedField when is_list(DecodedField) ->
         {DecodedField ++ DecodedFields, Context2, Data2};
      DecodedField ->
         {[DecodedField | DecodedFields], Context2, Data2}
   end.

decode_field(Data, Instr, Context = #context{pmap = <<>>}) ->
   decode_field(Data, Instr, Context#context{pmap = <<0:1>>});
decode_field(Data, Instr, Context) when is_record(Instr, string) ->
   %?debugFmt("Start to decode: pmap = ~p, ~p~n", [erlang_fast_utils:print_binary(Context#context.pmap), Instr]),
   Res = {Value, _Ctx, _Data1} = erlang_fast_string:decode(Data, Instr, Context),
   %?debugFmt("~p", [Value]),
   Res;
decode_field(Data, Instr, Context)
   when is_record(Instr, uInt32) or is_record(Instr, int32) or is_record(Instr, uInt64) or is_record(Instr, int64) ->
   %?debugFmt("Start to decode: pmap = ~p, ~p~n", [erlang_fast_utils:print_binary(Context#context.pmap), Instr]),
   Res = {Value, _Ctx, _Data1} = erlang_fast_number:decode(Data, Instr, Context),
   %?debugFmt("~p", [Value]),
   Res;
decode_field(Data, Instr, Context)
   when is_record(Instr, decimal) ->
   %?debugFmt("Start to decode: pmap = ~p, ~p~n", [erlang_fast_utils:print_binary(Context#context.pmap), Instr]),
   Res = {Value, _Ctx, _Data1} = erlang_fast_decimal:decode(Data, Instr, Context),
   %?debugFmt("~p", [Value]),
   Res;
decode_field(Data, Instr = #sequence{}, Context) ->
   erlang_fast_sequence:decode(Data, Instr, Context);

decode_field(Data, Instr = #group{}, Context) ->
   erlang_fast_group:decode(Data, Instr, Context);

% dynamic templateRef
decode_field(Data, #templateRef{name = undef}, Context) ->
   {Msg, #context{dicts = D}, Data1} = decode(Data, Context),
   {Msg, Context#context{dicts = D}, Data1};

%static templateRef
decode_field(Data, Instr = #templateRef{name = Name}, Context = #context{template = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(Name, Context#context.templates#templates.tlist),
   {skip, Data, Context#context.template#template{instructions = TemplateRef#template.instructions ++ Instrs}};

decode_field(Data, Instr, Context)  ->
   {Instr, Context, Data}.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

create_fake_context() ->
   F = fun([], _) -> ok;
          (Err, Val) -> io:format("~p: ~p~n", [Err, Val])
       end,
   erlang_fast:create_context("doc/templates.xml", F).

decode_test() ->
   Context = create_fake_context(),
   Data = <<
   16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#b3, 16#09, 16#4a,
   16#6c, 16#e9, 16#83, 16#ae, 16#82, 16#1c, 16#4e, 16#0e, 16#80, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#80,
   16#06, 16#47, 16#a1, 16#01, 16#bd, 16#9e, 16#81, 16#82, 16#79, 16#41, 16#91, 16#b9, 16#84, 16#b0, 16#81, 16#b1,
   16#06, 16#3f, 16#a1, 16#7e, 16#d2, 16#f0, 16#80, 16#01, 16#39, 16#14, 16#c3, 16#23, 16#5a, 16#2f, 16#5f, 16#2d,
   16#31, 16#42, 16#b4, 16#09, 16#4a, 16#6c, 16#e9, 16#81, 16#b1, 16#81, 16#b0, 16#81, 16#7a, 16#0c, 16#a6, 16#97,
   16#fa, 16#c0, 16#d4, 16#01, 16#39, 16#14, 16#c4, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09,
   16#4a, 16#6c, 16#e9, 16#81, 16#1e, 16#b0, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#84, 16#81, 16#00, 16#53,
   16#f9, 16#86, 16#83, 16#fe, 16#03, 16#2e, 16#90, 16#1c, 16#4e, 16#0e, 16#80, 16#83, 16#c0, 16#ed, 16#01, 16#39,
   16#14, 16#c5, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09, 16#4a, 16#6c, 16#e9, 16#82, 16#2d,
   16#e0, 16#82, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#85, 16#81, 16#00, 16#53, 16#f9, 16#8d, 16#1c, 16#4e,
   16#0e, 16#80, 16#00, 16#db, 16#80, 16#11, 16#c0, 16#b1, 16#82, 16#7f, 16#34, 16#ba, 16#9e, 16#00, 16#c6, 16#80,
   16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c6, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c8, 16#09, 16#4a,
   16#6c, 16#e9, 16#82, 16#93, 16#b1, 16#02, 16#34, 16#19, 16#87, 16#82, 16#7f, 16#34, 16#ba, 16#be, 16#90, 16#80,
   16#06, 16#3f, 16#d2, 16#86, 16#88, 16#80, 16#01, 16#39, 16#14, 16#c7, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31,
   16#42, 16#c8, 16#09, 16#4a, 16#6c, 16#e9, 16#82, 16#a0, 16#83, 16#80, 16#fe, 16#ff, 16#81, 16#81, 16#7a, 16#0b,
   16#fe, 16#f2, 16#fd, 16#80, 16#01, 16#39, 16#14, 16#c8, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c9,
   16#09, 16#4a, 16#6c, 16#e9, 16#81, 16#a1, 16#82, 16#82, 16#7f, 16#34, 16#b0, 16#96, 16#fd, 16#c0, 16#d4, 16#01,
   16#39, 16#14, 16#c9, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#44, 16#ad, 16#09, 16#4a, 16#6c, 16#e9, 16#81,
   16#0e, 16#90, 16#02, 16#34, 16#19, 16#8c, 16#81, 16#80, 16#fc, 16#83, 16#80, 16#82, 16#83, 16#c0, 16#ed, 16#01,
   16#39, 16#14, 16#ca, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#44, 16#ad, 16#09, 16#4a, 16#6c, 16#e9, 16#81>>,
   {T, {Msg1, Context1, Data1}} = timer:tc(?MODULE, decode, [Data, Context]),
   ?debugFmt("~p ~p~n", [T, Msg1]),
   {T1, {Msg2, _Context2, _Data2}} = timer:tc(?MODULE, decode, [Data1, Context1]),
   ?debugFmt("~p ~p~n", [T1, Msg2]).

-endif.
