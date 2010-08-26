-module(erlang_fast).

-include("erlang_fast_context.hrl").

-export
([
      create_context/1
      ,decode/2
]).

create_context(TemplatesFileName) ->
   Templates = erlang_fast_xml:parse(TemplatesFileName),
   #fast_context{templates = Templates}.

decode(Data, Context) ->
   ok.
