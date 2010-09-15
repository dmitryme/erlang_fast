-module(erlang_fast_group).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export(
   [
      decode/3
   ]).


decode(Data, #group{}, Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {group, Context#context{pmap = PMapRest}, Data}.
