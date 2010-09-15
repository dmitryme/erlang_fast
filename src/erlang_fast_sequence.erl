-module(erlang_fast_sequence).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-include_lib("eunit/include/eunit.hrl").

decode(Data, #sequence{name = FieldName, instructions = []}, Context) ->
   {{FieldName, absent}, Context, Data};

decode(Data, #sequence{name = FieldName, presence = Presence, need_pmap = NeedPMap, instructions = Instructions},
   Context = #fast_context{pmap = <<_:1, PMapRest/bitstring>>, template = T}) ->
   LenField = case is_record(hd(Instructions), length) of
      true ->
         LenInstr = hd(Instructions),
         #uInt32{name = LenInstr#length.name, presence = Presence, operator = LenInstr#length.operator};
      false ->
         #uInt32{name = "length", presence = Presence}
   end,
   {{_, LenValue}, Context1, Data1} = erlang_fast_number:decode(Data, LenField, Context),
   case LenValue of
      absent ->
         {{FieldName, absent},
            Context1#fast_context{pmap = PMapRest}, Data1};
      LenValue ->
         Instrs = case is_record(hd(Instructions), length) of
            true ->
               tl(Instructions);
            false ->
               Instructions
         end,
         {Sequence, #fast_context{dicts = Dicts}, Data2} = decode(LenValue, Data1, NeedPMap,
            Context1#fast_context{pmap = PMapRest, template = Context1#fast_context.template#template{instructions =
                  Instrs}}),
         {Sequence, Context1#fast_context{pmap = PMapRest, template = T, dicts = Dicts}, Data2}
   end;

decode(Data, #sequence{}, Context) ->
   {sequence, Context, Data}.

decode(0, Data, _NeedPMap, Context) ->
   {[], Context, Data};

decode(Length, Data, NeedPMap, Context = #fast_context{template = Template}) ->
   case NeedPMap of
      true ->
         {Context1 = #fast_context{pmap = <<_TBit:1, PMapRest/bitstring>>}, Data1} = erlang_fast_segment:decode_pmap(Data, Context),
         %?debugFmt("PMap = ~p", [erlang_fast_utils:print_binary(PMapRest)]),
         {Msg, Context2, Data2} = erlang_fast_segment:decode_fields(Data1, Context1),
         {Msgs, Context3, Data3} = decode(Length - 1, Data2, NeedPMap, Context2#fast_context{template = Template}),
         {[Msg | Msgs], Context3, Data3};
      false ->
         {Msg, Context1, Data1} = erlang_fast_segment:decode_fields(Data, Context),
         {Msgs, Context2, Data2} = decode(Length - 1, Data1, NeedPMap, Context1#fast_context{template = Template}),
         {[Msg | Msgs], Context2, Data2}
   end.
