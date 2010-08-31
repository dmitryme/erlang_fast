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

find_template_test() ->
   Context = create_fake_context(),
   ?assertMatch({template, "MDIncRefresh_83", _, 83, _, "83", _, _}, find_template(83, Context)).

-endif.
