-module(erlang_fast_utils).

-export(
   [
      find_template/2
      ,is_nullable/1
      ,apply_delta/3
      ,increment_value/3
   ]).

-include("include/erlang_fast_common.hrl").
-include("include/erlang_fast_context.hrl").
-include("include/erlang_fast_template.hrl").

find_template(Tid, Context = #fast_context{templates = Templates}) ->
   case gb_trees:lookup(Tid, Templates#templates.tlist) of
      none ->
         throw({'ERR D9', Tid, Context});
      {value, Template} ->
         Template
   end.

is_nullable(optional) -> true;
is_nullable(mandatory) -> false.

apply_delta(PrevVal, Len, _) when is_list(PrevVal) andalso (length(PrevVal) < abs(Len)) ->
  throw({'ERR D7', PrevVal});

apply_delta(PrevVal, Len, StrDelta) when is_list(PrevVal) andalso Len > 0 ->
  string:join([string:left(PrevVal, length(PrevVal) - Len), StrDelta], []);

apply_delta(PrevVal, Len, StrDelta) when is_list(PrevVal) andalso Len =< 0 ->
  string:join([StrDelta, string:right(PrevVal, length(PrevVal) + Len)], []).

increment_value(int32, Value, Inc) when Value + Inc >= 2147483647 ->
   -2147483648 + Inc;
increment_value(int64, Value, Inc) when Value + Inc >= 9223372036854775807 ->
   -9223372036854775808 + Inc;
increment_value(uInt32, Value, Inc) when Value + Inc >= 4294967295 ->
   Inc;
increment_value(uInt64, Value, Inc) when Value + Inc >= 18446744073709551615 ->
   Inc;
increment_value(uInt64, Value, Inc) ->
   Value + Inc.

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

apply_delta_test() ->
   ?assertEqual("abcdeab", apply_delta("abcdef", 1, "ab")),
   ?assertEqual("abcdab", apply_delta("abcdef", 2, "ab")),
   ?assertEqual("xycdef", apply_delta("abcdef", -2, "xy")),
   ?assertEqual("xyabcdef", apply_delta("abcdef", 0, "xy")),
   ?assertThrow({'ERR D7', "abcdef"}, apply_delta("abcdef", 7, "abc")),
   ?assertEqual("abxy", apply_delta("abcdef", 4, "xy")).

-endif.
