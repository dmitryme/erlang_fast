-module(erlang_fast_decode).

-export([decode_segment/2]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_decode_types,
   [
      decode_pmap/1
      ,decode_uint/2
   ]).

decode_segment(Data, Context) ->
   F = fun() ->
      {Context1, Rest1} = decode_pmap(Data, Context),
      {Context2, Rest2} = decode_template_id(Rest1, Context1),
      decode_template(Rest2, Context2)
   end,
   try F()
   catch
     _:Err ->
        Err
   end.

decode_template_id(Data, Context = #fast_context{dicts = Dicts, pmap = <<0:1, PMapRest/bitstring>>, logger = L}) -> %
   Tid = erlang_fast_dict:get_value(global, ?common_template_id_key, Dicts),
   case Tid of
      undef ->
         L('ERR D5', "Unable to get template ID from dictionary."),
         throw({'ERR D5', unable_to_know_template_id});
      empty ->
         L('ERR D6', "Template ID is empty in global dictionary."),
         throw({'ERR D6', unable_to_know_template_id});
      Tid ->
         Template = erlang_fast_utils:find_template(Tid, Context),
         {Context#fast_context{pmap = PMapRest, template = Template}, Data}
   end;

decode_template_id(Data, Context = #fast_context{pmap = <<1:1, PMapRest/bitstring>>, logger = L}) -> % tid is present into stream
   Res = erlang_fast_decode_types:decode_uint(Data, false),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Tid, Err, Rest} ->
         L(Err, Tid),
         Template = erlang_fast_utils:find_template(Tid, Context),
         {Context#fast_context{pmap = PMapRest, template = Template}, Rest}
   end.

decode_pmap(Data, Context = #fast_context{logger = L}) ->
   Res = erlang_fast_decode_types:decode_pmap(Data),
   case Res of
      not_enough_data ->
         throw({not_enough_data, Context});
      {Value, Err, Rest} ->
         L(Err, Value),
         {Context#fast_context{pmap = Value}, Rest}
   end.

decode_template(Data, Context = #fast_context{template = #template{instructions = []}}) ->
  {[], Context, Data};

decode_template(Data, Context = #fast_context{template = Template = #template{instructions = [Instr | Tail]}}) ->
  {DecodedField, Context1, Rest} = decode_instruction(
     Data,
     Instr,
     Context#fast_context{template = Template#template{instructions = Tail}}),
   {DecodedFields, Context2, Rest1} = decode_template(Rest, Context1),
  {[DecodedField | DecodedFields], Context2, Rest1}.

% ascii string decoding
decode_instruction(Data, {string, FieldName, _, _Id, _, ascii, _, #constant{value = InitialValue}}, Context) ->
   {{FieldName, InitialValue}, Context, Data};

decode_instruction(Data, {string, FieldName, _, _Id, optional, ascii, _, #default{value = InitialValue}},
      Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest}, Data};

decode_instruction(Data, {string, FieldName, _, _Id, Presence, ascii, _, #default{value = _InitialValue}},
     Context = #fast_context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>})
  when Presence == mandatory or (Presence == optional andalso PresenceBit == 1) ->
  Res = erlang_fast_decode_types:decode_string(Data),
  case Res of
     not_enough_data ->
        throw({not_enough_data, Context});
     {Value, Err, DataRest} ->
        L(Err, Value),
        case Presence of
           mandatory ->
              {{FieldName, Value}, Context, DataRest};
           optional ->
              {{FieldName, Value}, Context#fast_context{pmap = PMapRest}, DataRest}
        end
  end;

decode_instruction(Data, {string, FieldName, _, _, Presence, ascii, _, #copy{dictionary = Dict, key = Key}},
  Context = #fast_context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>, dicts = Dicts})
  when Presence == mandatory or (Presence == optional andalso PresenceBit == 1)->
  Res = erlang_fast_decode_types:decode_string(Data),
  case Res of
     not_enough_data ->
        throw({not_enough_data, Context});
     {Value, Err, DataRest} ->
        L(Err, Value),
        case Presence of
           mandatory ->
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
              {{FieldName, Value}, Context#fast_context{dicts = Dicts1}, DataRest};
           optional ->
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
              {{FieldName, Value}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest}
         end
  end;

decode_instruction(Data, {string, FieldName, _, _, optional, ascii, _, #copy{dictionary = Dict, key = Key, value = InitialValue}},
  Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts}) ->
  case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
     empty ->
        {FieldName, empty, Context};
     undef when InitialValue == undef -> % it becomes empty
        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
        {FieldName, empty, Context#fast_context{dicts = Dicts1}, Data};
     undef ->
        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
        {FieldName, InitialValue, Context#fast_context{dicts = Dicts1}, Data};
     Value ->
        {{FieldName, Value}, Context#fast_context{pmap = PMapRest}, Data}
  end;

decode_instruction(Data, {string, FieldName, _, _, _, ascii, _, _}, Context) ->
  {{FieldName, "<delta>"}, Context, Data}.

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

decode_segment_test() ->
   Context = create_fake_context(),
   Data = <<16#c0, 16#d3, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#3d, 16#31, 16#42, 16#b3>>,
   {Msg, #fast_context{pmap = Pmap}, _D} = decode_segment(Data, Context),
   ?debugFmt("~p ~p~n", [Pmap, Msg]).

-endif.
