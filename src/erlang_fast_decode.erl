-module(erlang_fast_decode).

-export([decode_segment/2]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

decode_segment(Data, Context) ->
   F = fun() ->
      {NewContext, Rest} = decode_pmap(Data, Context),
      {NewContext1, Rest1} = decode_template_id(Rest, NewContext),
      decode_message(Rest1, NewContext1),
      {NewContext1, Rest1}
   end,
   try F()
   catch
      _:Err ->
         Err
   end.

decode_template_id(Data, Context = #fast_context{logger = L}) ->
   Res = erlang_fast_decode_types:decode_uint(Data, false),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Tid, Err, Rest} ->
         Template = erlang_fast_utils:find_template(Tid, Context),
         L(Err, Res),
         {Context#fast_context{template = Template}, Rest}
   end.

decode_pmap(Data, Context = #fast_context{logger = L}) ->
   Res = erlang_fast_decode_types:decode_pmap(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Value, Err, Rest} ->
         L(Err, Res),
         {Context#fast_context{pmap = Value}, Rest}
   end.

decode_template(_, #fast_context{template = #template{instructions = []}}) ->
   [];
decode_template(Data, #fast_context{template = #template{instructions = [Instr | Rest]}}) ->
   {DecodedField, Context, Rest} = decode_instruction(
      Data,
      Instr,
      #fast_context{template = #template{instructions = Rest}}),
   {[DecodedField | decode_template(Rest, Context)], Data, Context}.

decode_instruction(_, _, _) -> ok.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

decode_segment_test() ->
   Templates = erlang_fast_xml:parse("doc/templates.xml"),
   F = fun([], _) -> ok;
          (Err, Val) -> io:format("~p: ~p~n", [Err, Val])
       end,
   Context = #fast_context{templates = Templates, logger = F},
   Data = <<16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#3d, 16#31, 16#42, 16#b3>>,
   ?assertEqual(null, decode_segment(Data, Context)).

-endif.
