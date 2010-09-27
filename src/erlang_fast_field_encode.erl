-module(erlang_fast_field_encode).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export(
   [
      encode/3
   ]).

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/2
      ,increment_value/3
      ,select_dict/3
      ,get_delta/2
   ]).

-import(erlang_fast_encode_types,
   [
      encode_type/3
      ,encode_delta/3
   ]).

-include_lib("eunit/include/eunit.hrl").

%% =========================================================================================================
%% encoding
%% =========================================================================================================

encode(MsgFields = [{FieldName1, _} | MsgFieldsRest],
   I = #field{name = FieldName2, presence = Presence, operator = #constant{}},
      Context = #context{pmap = PMap}) ->
?debugFmt("~p ~p", [MsgFields, I]),
   case Presence of
      mandatory ->
         {<<>>, MsgFieldsRest, Context};
      optional when FieldName1 == FieldName2 ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>}};
      optional when FieldName1 =/= FieldName2 ->
         {<<>>, MsgFields, Context#context{pmap = <<PMap/bitstring, 0:1>>}}
   end;

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   I = #field{type = Type, name = FieldName2, presence = Presence, operator = #default{value = InitialValue}},
      Context = #context{pmap = PMap}) ->
?debugFmt("~p ~p", [MsgFields, I]),
   case Presence of
      optional when (FieldName1 =/= FieldName2) ->
         {encode_type(Type, null, is_nullable(Presence)), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>}}
   end;

encode(MsgFields = [{FieldName1, _} | _], I = #field{type = Type, name = FieldName2, presence = optional, operator = #copy{}},
   Context = #context{pmap = PMap}) when FieldName1 =/= FieldName2 ->
?debugFmt("~p ~p", [MsgFields, I]),
   {encode_type(Type, null, true), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   I = #field{type = Type, name = FieldName2, presence = Presence, operator = #copy{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
?debugFmt("~p ~p", [MsgFields, I]),
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}};
      undef when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};
      Value when (FieldName1 == FieldName2)->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>}};
      DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}}
   end;

encode(MsgFields = [{FieldName1, _} | _], #field{type = Type, name = FieldName2, presence = optional, operator = #delta{}}, Context)
   when FieldName1 =/= FieldName2 ->
   {encode_type(Type, null, true), MsgFields, Context};

encode([{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef when (FieldName1 == FieldName2) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}};
      Value when (FieldName1 == FieldName2)->
         {<<>>, MsgFieldsRest, Context};
      DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}}
   end;

encode(MsgFields = [{FieldName1, _} | _], #field{type = Type, name = FieldName2, presence = optional, operator = #tail{}},
   Context = #context{pmap = PMap}) when FieldName1 =/= FieldName2 ->
   {encode_type(Type, null, true), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

%encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
%   Instr = #field{type = Type, name = FieldName2, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
%      Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
%   Dict = select_dict(D, TemplateName, App),
%   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
%      undef when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
%         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
%         {encode_delta(Type, get_delta(alue, InitialValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}};
%      undef when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
%         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%         {encode_delta(Type, get_delta(Value, InitialValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}};
%      Value when (FieldName1 == FieldName2)->
%         {<<>>, MsgFieldsRest, Context};
%      DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
%         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%         {encode_delta(Type, get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}}
%   end.

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = undef}, Context) ->
   case FieldName1 == FieldName2 of
      true ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context};
      false ->
         {encode_type(Type, null, true), MsgFields, Context}
   end;

encode(Msgs, Instr, Context) ->
   {<<>>, Msgs, Context}.

%encode(_, Instr, _Context) ->
%   throw({error, [unknown_field, Instr]}).
