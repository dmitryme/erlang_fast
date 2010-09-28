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

%% =========================================================================================================
%% constant
%% =========================================================================================================

encode(MsgFields = [{FieldName1, _} | MsgFieldsRest],
   #field{name = FieldName2, presence = Presence, operator = #constant{}},
      Context = #context{pmap = PMap}) ->
   case Presence of
      mandatory ->
         {<<>>, MsgFieldsRest, Context};
      optional when FieldName1 == FieldName2 ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>}};
      optional when FieldName1 =/= FieldName2 ->
         {<<>>, MsgFields, Context#context{pmap = <<PMap/bitstring, 0:1>>}}
   end;

%% =========================================================================================================
%% default
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = #default{value = InitialValue}},
      Context = #context{pmap = PMap}) ->
   case Presence of
      optional when (FieldName1 =/= FieldName2) ->
         {<<>>, MsgFields, Context#context{pmap = <<PMap/bitstring, 0:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>}}
   end;

%% =========================================================================================================
%% copy
%% =========================================================================================================

encode([{FieldName1, _} | _],#field{name = FieldName2, presence = mandatory, operator =
      #copy{}}, _Context) when FieldName1 =/= FieldName2 ->
   throw({error, ['ERR D6', FieldName2, "Mandatory field can not be absent with copy operator."]});

encode(MsgFields = [{FieldName1, _} | _],
   #field{type = Type, name = FieldName2, presence = optional, operator = #copy{dictionary = D, key = Key}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}})
   when FieldName1 =/= FieldName2 ->
   Dicts1 = erlang_fast_dicts:put_value(select_dict(D, TemplateName, App), Key, empty, Dicts),
   {encode_type(Type, null, true), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #copy{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};
      undef when (Value == InitialValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}};
      undef when (Value =/= InitialValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>}};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}}
   end;

%% =========================================================================================================
%% delta
%% =========================================================================================================

encode(MsgFields = [{FieldName1, _} | _], #field{type = Type, name = FieldName2, presence = Presence, operator = #delta{}}, Context)
   when FieldName1 =/= FieldName2 ->
   {encode_type(Type, null, Presence), MsgFields, Context};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#context{dicts = Dicts1}}
   end;

%% =========================================================================================================
%% tail
%% =========================================================================================================

encode(MsgFields = [{FieldName1, _} | _], #field{type = Type, presence = Presence, name = FieldName2, operator = #tail{}},
   Context = #context{pmap = PMap}) when FieldName1 =/= FieldName2 ->
   {encode_type(Type, null, Presence), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), Presence), MsgFieldsRest,
            Context#context{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>}};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#context{pmap =
               <<PMap/bitstring, 1:1>>, dicts = Dicts1}}
   end;

%% =========================================================================================================
%% increment
%% =========================================================================================================

encode(MsgFields = [{FieldName1, _} | _], #field{type = Type, presence = Presence, name = FieldName2, operator =
      #increment{}},
   Context = #context{pmap = PMap}) when FieldName1 =/= FieldName2 ->
   {encode_type(Type, null, Presence), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #increment{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef when (InitialValue == undef) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>,
            dicts = Dicts1}};
      undef when (Value - InitialValue == 1)->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}};
      DictValue when (Value - DictValue == 1) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}};
      _ ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}}
   end;

%% =========================================================================================================
%% undef
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = undef}, Context) ->
   case FieldName1 == FieldName2 of
      true ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context};
      false ->
         {encode_type(Type, null, true), MsgFields, Context}
   end;

%% =========================================================================================================
%% typeRef
%% =========================================================================================================

encode(MsgFields, #typeRef{name = AppName}, Context) ->
   {<<>>, MsgFields, Context#context{application = AppName}};

%% =========================================================================================================
%% templateRef
%% =========================================================================================================

encode([{Tid, MsgRefFields} | MsgFieldsRest], #templateRef{name = undef}, Context = #context{pmap = PMap}) ->
   TemplateRef = erlang_fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
   {TemplRefBin, _, Context1} = erlang_fast_segment:encode(MsgRefFields, Context#context{template = TemplateRef}),
   {TemplRefBin, MsgFieldsRest,
      Context#context{pmap = <<PMap/bitstring, (Context1#context.pmap)/bitstring>>, dicts = Context1#context.dicts}};

encode(MsgFields, #templateRef{name = TemplateName}, Context = #context{template = T = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(TemplateName, Context#context.templates#templates.tlist),
   {<<>>, MsgFields, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

%% =========================================================================================================
%% group
%% =========================================================================================================
%encode(MsgFields = [{Tid, MsgRefFields} | MsgFieldsRest],
%   #field_group{type = group, name = GroupName, need_pmap = MeedPMap, dictionary = D, instructions = Instrs},
%      Context}) ->

encode(Msgs, _Instr, Context) ->
   {<<>>, Msgs, Context}.

%encode(_, Instr, _Context) ->
%   throw({error, [unknown_field, Instr]}).
