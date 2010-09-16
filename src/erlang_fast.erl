-module(erlang_fast).

-include("erlang_fast_context.hrl").

-export
([
      create_context/2
      ,decode/2
]).

create_context(TemplatesFileName, Logger) ->
   {Dicts, Templates} = erlang_fast_xml:parse(TemplatesFileName),
   #context{dicts = Dicts, templates = Templates, logger = Logger}.

decode(Data, Context) ->
   erlang_decode_segment:decode(Data, Context).
