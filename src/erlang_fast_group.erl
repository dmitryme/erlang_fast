-module(erlang_fast_group).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export(
   [
      decode/3
   ]).


decode(Data, #group{name = FieldName}, Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};

decode(Data, #group{name = FieldName, need_pmap = NeedPMap, instructions = Instrs},
   Context = #context{pmap = <<1:1, PMapRest/bitstring>>}) ->
   {Data1, Context1} =
   case NeedPMap of
      true ->
         erlang_fast_segment:decode_pmap(Data, Context);
      false ->
         {Data, Context}
   end,
   {Msg, Data2, #context{dicts = Dicts}} = erlang_fast_segment:decode_fields(Data1, Context1#context.template#template{instructions =
         Instrs}),
   {{FieldName, Msg}, Data2, Context#context{pmap = PMapRest, dicts = Dicts}};

decode(_, Instr, _) ->
   throw({error, [unknown_group_type, Instr]}).
