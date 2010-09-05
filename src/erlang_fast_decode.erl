-module(erlang_fast_decode).

-export(
   [
      decode_segment/2
      ,decode_template/2
      ,decode_pmap/2
      ,decode_template_id/2
   ]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

-compile([export_all]).

decode_segment(Data, Context) ->
   F = fun() ->
      {T1, {Context1, Rest1}} = timer:tc(erlang_fast_decode, decode_pmap, [Data, Context]),
      {T2, {Context2, Rest2}} = timer:tc(erlang_fast_decode, decode_template_id, [Rest1, Context1]),
      {T3, Res} = timer:tc(erlang_fast_decode, decode_template, [Rest2, Context2]),
      {[T1, T2, T3], Res}
   end,
   F().
   %try F()
   %catch
   %  _:Err ->
   %     Err
   %end.

decode_template_id(Data, Context = #fast_context{dicts = Dicts, pmap = <<0:1, PMapRest/bitstring>>, logger = L}) -> %
   Tid = erlang_fast_dict:get_value(global, ?common_template_id_key, Dicts),
   case Tid of
      undef ->
         L('ERR D5', "Unable to get template ID from dictionary."),
         throw({'ERR D5', unable_to_know_template_id});
      empty ->
         L('ERR D6', "Template ID is empty in global dictionary."),
         throw({'ERR D6', unable_to_know_template_id});
      Tid ->
         Template = erlang_fast_utils:find_template(Tid, Context),
         {Context#fast_context{pmap = PMapRest, template = Template}, Data}
   end;

decode_template_id(Data, Context = #fast_context{pmap = <<1:1, PMapRest/bitstring>>, logger = L}) -> % tid is present into stream
   Res = erlang_fast_decode_types:decode_uint(Data, false),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Tid, Err, Rest} ->
         L(Err, Tid),
         Template = erlang_fast_utils:find_template(Tid, Context),
         {Context#fast_context{pmap = PMapRest, template = Template}, Rest}
   end.

decode_pmap(Data, Context = #fast_context{logger = L}) ->
   Res = erlang_fast_decode_types:decode_pmap(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Value, Err, Rest} ->
         L(Err, Value),
         {Context#fast_context{pmap = Value}, Rest}
   end.

decode_template(Data, Context = #fast_context{template = #template{instructions = []}}) ->
  {[], Context, Data};

decode_template(Data, Context = #fast_context{template = Template = #template{instructions = [Instr | Tail]}}) ->
   {DecodedField, Context1, Rest} = decode_instruction(
      Data,
      Instr,
      Context#fast_context{template = Template#template{instructions = Tail}}),
   {DecodedFields, Context2, Rest1} = decode_template(Rest, Context1),
   case DecodedField of
      {_FieldName, absent} ->
         {DecodedFields, Context2, Rest1};
      DecodedField ->
         {[DecodedField | DecodedFields], Context2, Rest1}
   end.

decode_instruction(Data, Instr, Context) when is_record(Instr, string) ->
   erlang_fast_decode_string:decode_instruction(Data, Instr, Context);
decode_instruction(Data, Instr, Context)
  when is_record(Instr, uInt32) or is_record(Instr, int32) or is_record(Instr, uInt64) or is_record(Instr, int64) ->
  erlang_fast_decode_number:decode_instruction(Data, Instr, Context);
decode_instruction(Data, Instr, Context) when is_record(Instr, sequence)  ->
   erlang_fast_decode_sequence:decode_instruction(Data, Instr, Context);
decode_instruction(Data, Instr, Context)  ->
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

decode_segment_test() ->
   Context = create_fake_context(),
   Data = <<16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#b3,
            16#09, 16#4a, 16#6c, 16#e9, 16#83, 16#ae, 16#82, 16#1c, 16#4e, 16#0e, 16#80, 16#01, 16#50, 16#da,
            16#02, 16#34, 16#19, 16#80, 16#06, 16#47, 16#a1, 16#01, 16#bd, 16#9e, 16#81, 16#82, 16#79, 16#41,
            16#91, 16#b9, 16#84, 16#b0, 16#81, 16#b1, 16#06, 16#3f, 16#a1, 16#7e, 16#d2, 16#f0, 16#80, 16#01,
            16#39, 16#14, 16#c3, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#b4, 16#09, 16#4a, 16#6c>>,
   {PTime, {TT, {Msg, #fast_context{pmap = _Pmap}, _D}}} = timer:tc(erlang_fast_decode, decode_segment, [Data, Context]),
   ?debugFmt("~p ~p ~p~n", [PTime, TT, Msg]).

-endif.
