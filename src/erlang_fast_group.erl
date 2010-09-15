-module(erlang_fast_group).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export(
   [
      decode/3
   ]).


decode(Data, #group{}, Context) ->
   {group, Context, Data}.
