-module(erlang_fast_utils).

-export([find_template/2]).

-include("include/erlang_fast_common.hrl").
-include("include/erlang_fast_context.hrl").
-include("include/erlang_fast_template.hrl").

find_template(Tid, Context = #fast_context{templates = Templates}) ->
   case lists:keyfind(Tid, ?FieldId(template, id), Templates#templates.tlist) of
      false ->
         throw({'ERR D9', Tid, Context});
      Template ->
         Template
   end.