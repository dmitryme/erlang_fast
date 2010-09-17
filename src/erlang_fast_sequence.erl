-module(erlang_fast_sequence).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-include_lib("eunit/include/eunit.hrl").

decode(Data, #sequence{name = FieldName, instructions = []}, Context) ->
   {{FieldName, absent}, Data, Context};

decode(Data, #sequence{name = FieldName, presence = Presence, need_pmap = NeedPMap, instructions = Instructions},
   Context = #context{pmap = <<_:1, PMapRest/bitstring>>}) ->
   LenField = case is_record(hd(Instructions), length) of
      true ->
         LenInstr = hd(Instructions),
         #uInt32{name = LenInstr#length.name, presence = Presence, operator = LenInstr#length.operator};
      false ->
         #uInt32{name = "length", presence = Presence}
   end,
   {{_, LenValue}, Data1, Context1} = erlang_fast_number:decode(Data, LenField, Context),
   case LenValue of
      absent ->
         {{FieldName, absent}, Data1, Context1#context{pmap = PMapRest}};
      LenValue ->
         Instrs = case is_record(hd(Instructions), length) of
            true ->
               tl(Instructions);
            false ->
               Instructions
         end,
         {Sequence, Data2, #context{dicts = Dicts}} = decode(LenValue, Data1, NeedPMap,
            Context1#context{pmap = PMapRest, template = Context1#context.template#template{instructions =
                  Instrs}}),
         {{FieldName, Sequence}, Data2, Context#context{pmap = PMapRest, dicts = Dicts}}
   end;

decode(_Data, Instr, _Context) ->
   throw({error, [unknown_sequence_type, Instr]}).


decode(0, Data, _NeedPMap, Context) ->
   {[], Data, Context};

decode(Length, Data, NeedPMap, Context = #context{template = Template}) ->
   case NeedPMap of
      true ->
         {Data1, Context1} = erlang_fast_segment:decode_pmap(Data, Context),
         {Msg, Data2, Context2} = erlang_fast_segment:decode_fields(Data1, Context1),
         {Msgs, Data3, Context3} = decode(Length - 1, Data2, NeedPMap, Context2#context{template = Template}),
         {[Msg | Msgs], Data3, Context3};
      false ->
         {Msg, Data1, Context1} = erlang_fast_segment:decode_fields(Data, Context),
         {Msgs, Data2, Context2} = decode(Length - 1, Data1, NeedPMap, Context1#context{template = Template}),
         {[Msg | Msgs], Data2, Context2}
   end.
