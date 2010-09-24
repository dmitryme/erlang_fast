-module(erlang_fast).

-include("erlang_fast_context.hrl").

-export
([
      create_context/2
      ,reset/1
      ,decode/2
      ,encode/2
   ]).

create_context(TemplatesFileName, Logger) ->
   {Dicts, Templates} = erlang_fast_xml:parse(TemplatesFileName),
   #context{dicts = Dicts, templates = Templates, logger = Logger}.

reset(Context = #context{dicts = Dicts}) ->
   Dicts1 = gb_trees:map(fun(_K, _V) -> undef end, Dicts),
   Context#context{dicts = Dicts1}.

decode(Data, Context) ->
   erlang_fast_segment:decode(Data, Context).

encode(Msg, Context) ->
   erlang_fast_segment:encode(Msg, Context).
