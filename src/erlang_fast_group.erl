-module(erlang_fast_group).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export(
   [
      decode/3
   ]).


decode(Data, #group{name = FieldName}, Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Context#context{pmap = PMapRest}, Data};

decode(Data, #group{name = FieldName, need_pmap = NeedPMap, instructions = Instrs},
   Context = #context{pmap = <<1:1, PMapRest/bitstring>>}) ->
   {Context1, Data1} =
   case NeedPMap of
      true ->
         erlang_fast_segment:decode_pmap(Data, Context);
      false ->
         {Context, Data}
   end,
   {Msg, #context{dicts = Dicts}, Data2} = erlang_fast_segment:decode_fields(Data1, Context1#context.template#template{instructions =
         Instrs}),
   {{FieldName, Msg}, Context#context{pmap = PMapRest, dicts = Dicts}, Data2}.
