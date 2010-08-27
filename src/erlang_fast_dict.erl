-module(erlang_fast_dict).

-export([
      init/0
      ,new_dict/2
      ,get_value/3
      ,put_value/4
      ,reset/0
   ]).

init() ->
   [].

new_dict(Name, Dicts) ->
   case lists:keyfind(Name, 1, Dicts) of
      false ->
         [{Name, dict:new()} | Dicts];
      _ ->
         Dicts
   end.

get_value(DictName, Key, Dicts) ->
   case lists:keyfind(DictName, 1, Dicts) of
      false ->
         throw({dictionary_not_found, DictName});
      {DictName, Dict} ->
         case dict:find(Key, Dict) of
            {ok, Value} ->
               Value;
            error ->
               not_found
         end
   end.

put_value(DictName, Key, Value, Dicts) ->
   case lists:keyfind(DictName, 1, Dicts) of
      false ->
         throw({dictionary_not_found, DictName});
      {DictName, Dict} ->
         Dict1 = dict:store(Key, Value, Dict),
         lists:keyreplace(DictName, 1, Dicts, {DictName, Dict1})
   end.

reset() ->
   ok.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

dict_test() ->
   Dicts = init(),
   Dicts1 = new_dict("1", Dicts),
   Dicts2 = put_value("1", "12", 123, Dicts1),
   ?assertEqual(123, get_value("1", "12", Dicts2)),
   Dicts3 = new_dict("2", Dicts2),
   Dicts4 = put_value("2", "abc", "test", Dicts3),
   ?assertEqual("test", get_value("2", "abc", Dicts4)).

-endif.
