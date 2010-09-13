-module(erlang_fast_sequence).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

decode(Data, #sequence{name = FieldName, presence = Presence, need_pmap = NeedPMap, instructions = Instructions},
      Context = #fast_context{pmap = <<_:1, PMapRest/bitstring>>, template = #template{instructions = TempInstr}}) ->
   LenField = case is_record(hd(TempInstr), length) of
      true ->
         LenInstr = hd(TempInstr),
         #uInt32{name = LenInstr#length.name, presence = Presence, operator = LenInstr#length.operator};
      false ->
         #uInt32{name = "length", presence = Presence}
   end,
   {{_, LenValue}, Context1, Data1} = erlang_fast_number:decode(Data, LenField, Context),
   case LenValue of
     absent ->
         Instrs = case is_record(hd(TempInstr), length) of
                     true ->
                        tl(TempInstr);
                     false ->
                        TempInstr
                     end,
        {{FieldName, absent},
           Context1#fast_context{pmap = PMapRest, template = Context1#fast_context.template#template{instructions = Instrs}}, Data1};
     LenValue ->
        decode(LenValue, Data1, NeedPMap,
           Context1#fast_context{pmap = PMapRest, template = Context1#fast_context.template#template{instructions = Instructions}})
  end;

decode(Data, #sequence{}, Context) ->
  {sequence, Context, Data}.

decode(0, Data, _NeedPMap, Context) ->
   {[], Context, Data};

decode(Length, Data, NeedPMap, Context) ->
   case NeedPMap of
      true ->
         {Context1, Data1} = erlang_fast_segment:decode_pmap(Data, Context),
         {Msg, Context2, Data2} = erlang_fast_segment:decode_fields(Data1, Context1),
         {Msgs, Context3, Data3} = decode(Length, Data2, NeedPMap, Context2),
         {[Msg | Msgs], Context3, Data3};
      false ->
         {Msg, Context1, Data1} = erlang_fast_segment:decode_fields(Data, Context),
         {Msgs, Context2, Data2} = decode(Length, Data1, NeedPMap, Context1),
         {[Msg | Msgs], Context2, Data2}
   end.
