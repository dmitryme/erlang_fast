-module(erlang_fast).

-include("erlang_fast_context.hrl").

-export
([
      create_context/3
      ,reset_context/1
      ,decode/2
      ,encode/3
   ]).

% Msg = {TemplateId, Fields()}
% Fields = [Field]
% Field = {FieldName, Value} | {TemplateId, Fields}
% Value = Number() | binary() | [Fields]
% FieldName = string()
% TemplateId = Number()

% create_context(TemplatesDescr, Options, Logger) -> {ok, Context}
%  TemplatesDescr = {file, TemplatesFilename}, TemplatesXmlText
%  Options = [Option]
%  Option = use_id - use field id instead of field name in decoded field tuple
%           force_encode_tid - always encode template ID, even if previous template ID is the same
%  TemplatesFilename = String() - path to XML file with templates definitions
%  TemplatesXmlText = String() - XML text with templates definitions
%  Logger - is a callback logger function with arity 2, where first param can be
%             list of Error numbers or [] - if no error, and second parameter is a list with error details
create_context(TemplatesDescr, Options, Logger) ->
   try
      {Dicts, Templates} = erlang_fast_xml:parse(TemplatesDescr, Options),
      {ok, #context{dicts = Dicts, templates = Templates, logger = Logger, options = Options}}
   catch
      _:Err ->
         Err
   end.

% reset_context(Context) -> {ok, Context}
%  resets context
reset_context(Context = #context{dicts = Dicts}) ->
   Dicts1 = gb_trees:map(fun(_K, _V) -> [] end, Dicts),
   {ok, Context#context{dicts = Dicts1}}.

% decode(Data, Context) -> {ok, {Name, Msg, DataRest, NewContext}} | {error, Reason}
%  Data = binary()
%  Context = Context()
%  Name = term()
%  DataRest = binary()
%  NewContext = Context()
decode(Data, Context) ->
   erlang_fast_segment:decode(Data, Context).

% encode(Msg, Context) -> {ok, {Data, NewContext}} | {error, Reason}
%  TemplateId - number() - template ID
%  Msg = Msg()
%  Context  = Context()
%  Data = binary()
%  NewContext = Context()
encode(TemplateId, Msg, Context) ->
   erlang_fast_segment:encode(TemplateId, Msg, Context).
