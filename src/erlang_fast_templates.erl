-module(erlang_fast_templates).

-include("erlang_fast_template.hrl").

-export([
      init/0
      ,add_template/2
      ,get_by_name/2
      ,get_by_id/2
   ]).

init() ->
   {gb_trees:empty(), gb_trees:empty()}.

add_template(T = #template{name = Name, id = Id}, {TemplsByName, TemplsByID}) ->
   NewTemplsByName = gb_trees:enter(Name, T, TemplsByName),
   NewTemplsByID =
   case Id of
      undef ->
         TemplsByID;
      _ ->
         gb_trees:enter(Id, T, TemplsByID)
   end,
   {NewTemplsByName, NewTemplsByID}.

get_by_name(TemplName, {TemplsByName, _}) ->
   case gb_trees:lookup(TemplName, TemplsByName) of
      none ->
         throw({error, ['ERR D8', TemplName, "no such template"]});
      {value, Templ} ->
         Templ
   end.

get_by_id(TemplID, {_, TemplsByID}) ->
   case gb_trees:lookup(TemplID, TemplsByID) of
      none ->
         throw({error, ['ERR D9', TemplID, "no such template"]});
      {value, Templ} ->
         Templ
   end.
