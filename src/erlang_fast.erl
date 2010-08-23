-module(erlang_fast).

-include("erlang_fast_context.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

-export
([
      create_context/1
      ,decode_segment/2
]).


create_context(TemplatesFileName) ->
   Templates = erlang_fast_xml:parse(TemplatesFileName),
   #fast_context{templates = Templates}.

decode_segment(Data, Context) ->
   F = fun() ->
      {NewContext1, Rest} = decode_pmap(Data, Context),
      {NewContext2, Rest1} = decode_template_id(Rest, NewContext1),
      decode_message(Rest1, NewContext2)
   end,
   try F()
   catch
      _:Err ->
         Err
   end.

decode_template_id(Data, Context) ->
   Res = erlang_fast_decode_types:decode_uint(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Tid, Err, Rest} ->
         case lists:keyfind(Tid, 4, Context#fast_context.templates) of
            false ->
               throw({'ERR D9', Tid, Context});
            Template ->
               {Context#fast_context{template = Template}, Rest}
         end
   end.

decode_pmap(Data, Context) ->
   Res = erlang_fast_decode_types:decode_pmap(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Value, Rest} ->
         {Context#fast_context{pmap = Value}, Rest}
   end.

decode_message(Data, Context) ->
   ok.
