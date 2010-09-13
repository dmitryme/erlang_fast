-module(erlang_fast_dicts).

-export([
      init/0
      ,new_dict/2
      ,get_value/3
      ,put_value/4
      ,reset/0
   ]).

init() ->
   gb_trees:empty().

new_dict(Name, Dicts) ->
   case gb_trees:lookup(Name, Dicts) of
      none ->
         gb_trees:insert(Name, dict:new(), Dicts);
      {value, _} ->
         Dicts
   end.

get_value(DictName, Key, Dicts) ->
   case gb_trees:lookup(DictName, Dicts) of
      none ->
         throw({dictionary_not_found, DictName});
      {value, Dict} ->
         case dict:find(Key, Dict) of
            {ok, Value} ->
               Value;
            error ->
               undef
         end
   end.

put_value(DictName, Key, Value, Dicts) ->
   case gb_trees:lookup(DictName, Dicts) of
      none ->
         throw({dictionary_not_found, DictName});
      {value, Dict} ->
         Dict1 = dict:store(Key, Value, Dict),
         gb_trees:update(DictName, Dict1, Dicts)
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
