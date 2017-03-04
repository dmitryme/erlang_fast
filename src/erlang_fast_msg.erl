-module(erlang_fast_msg).

-include("erlang_fast_context.hrl").

-export([new/1, add/3, add_grp/3, merge/2, get/3, reverse/1]).

new(Context) ->
   case proplists:get_bool(use_map, Context#context.options) of
      true ->
         #{};
      false ->
         []
   end.

add(Msg, Tag, Value) when is_map(Msg) ->
   Msg#{Tag => Value};

add(Msg, Tag, Value) when is_list(Msg) ->
   [{Tag, Value}|Msg].

add_grp(Msg, Idx, Value) when is_map(Msg) ->
   Msg#{Idx => Value};

add_grp(Msg, _Idx, Value) when is_list(Msg) ->
   [Value|Msg].

merge(Msg1, Msg2) when is_map(Msg1) ->
   maps:merge(Msg1, Msg2);

merge(Msg1, Msg2) when is_list(Msg1) ->
   lists:append(Msg1, Msg2).

get(Msg, Tag, Def) when is_map(Msg) ->
   maps:get(Tag, Msg, Def);

get(Msg, Tag, Def) when is_list(Msg) ->
   proplists:get_value(Tag, Msg, Def).

reverse(Msg) when is_list(Msg) ->
   lists:reverse(Msg);

reverse(Msg) when is_map(Msg) ->
   Msg.
