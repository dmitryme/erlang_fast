%string decoding

-module(erlang_fast_string).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-export
   ([
      decode/3
      %,encode/3
   ]).

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/2
      ,apply_delta/3
      ,select_dict/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_string/2
      ,decode_string_delta/2
      ,decode_vector/2
      ,decode_vector_delta/2
   ]).

%% =========================================================================================================
%% decoding
%% =========================================================================================================

decode(Data, {_, FieldName, _, _, Presence, #constant{value = InitialValue}},
   Context = #context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
   when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
      case Presence of
         mandatory ->
            {{FieldName, InitialValue}, Data, Context};
         optional ->
            {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
      end;

decode(Data, {_, FieldName, _, _, optional, #constant{}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};

decode(Data, {_, FieldName, _, _, _, #default{value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   case InitialValue of
      undef ->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};
      InitialValue ->
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #default{value = _InitialValue}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>}) ->
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest}};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest}}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #copy{dictionary = D, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
   end;

decode(Data, {_Type, FieldName, _, _, Presence, #copy{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when Presence == mandatory ->
         throw({error, ['ERR D6', FieldName, "Previous value is empty for mandatory field"]});
      empty when Presence == optional ->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};
      undef when (Presence == mandatory) and (InitialValue == undef) -> % ERR D5
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {{FieldName, Value}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   case decode_string_delta(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {StringDelta = {Len, Delta}, Err, Data1} ->
         L(Err, StringDelta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, ['ERR D6', FieldName, "Previous value is empty for delta operator"]});
            undef ->
               NewVal = apply_delta(InitialValue, Len, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Len, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}}
         end
   end;

%% ======================================================================================================
%% tail decoding
%% ======================================================================================================

decode(Data, {Type, FieldName, _, _, Presence, #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, pmap = <<1:1, PMapRest/bitstring>>,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            PrevValue when (PrevValue == empty) or (PrevValue == undef)->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
         end
   end;

decode(Data, {_Type, FieldName, _, _, Presence, #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bitstring>>,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, ['ERR D7', FieldName, "Previous value is empty for mandatory field"]});
      empty when (Presence == optional)->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};
      undef when (InitialValue == undef) andalso (Presence == mandatory) ->
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (InitialValue == undef) andalso (Presence == optional) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {{FieldName, Value}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {Type, FieldName, _, _, Presence, undef}, Context = #context{logger = L}) ->
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context}
   end;

decode(_, Instr, _) ->
   throw({error, [unknown_string_type, Instr]}).

%% ==========================================================================================
%% helpers
%% ==========================================================================================

decode_string(string, Data, Nullable) ->
   decode_string(Data, Nullable);
decode_string(Type, Data, Nullable) when (Type == unicode) or (Type == byteVector) ->
   decode_vector(Data, Nullable).

decode_string_delta(string, Data, Nullable) ->
   decode_string_delta(Data, Nullable);
decode_string_delta(Type, Data, Nullable) when (Type == unicode) or (Type == byteVector) ->
   decode_vector_delta(Data, Nullable).

%% =========================================================================================================
%% encoding
%% =========================================================================================================

%encode([{FieldName, _} | MsgFieldsRest], {_, FieldName, _, _, mandatory, {constant, _}}, Context) ->
%  {<<>>, MsgFieldsRest, Context};

%encode(MsgFields = [{FieldName1, _} | MsgFieldsRest], {_, FieldName2, _, _, optional, {constant, _}}, Context = #context{pmap = PMap}) ->
%  case FieldName1 == FieldName2 of
%     true -> % the field is presend in message
%        {<<>>, MsgFieldsRest, Context#{pmap = <<PMap/bitstring, 1:1>>}};
%     false -> % the field is absent
%        {<<>>, MsgFields, Context#{pmap = <<PMap/bitstring, 0:1>>}}
%  end;

%encode([{FieldName, InitialValue} | MsgFieldsRest], {_, FieldName, _, _, _Presence, {default, InitialValue}},
%  Context = #context{pmap = PMap}) ->
%  {<<>>, MsgFieldsRest, Context#context{pmap = <<Pmap/bitstring, 0:1>>}};

%encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest], {_, FieldName2, _, _, Presence, {default, InitialValue}},
%  Context = #context{pmap = PMap}) when Value =/= InitialValue ->
%  case FieldName1 == FieldName2 of
%     false when Presence == optional ->
%        {encode_string(null, Presence)), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>};
%     true ->
%        {encode_string(Value, Presence), MsgFieldsRest, Context#context{pmap = <<PMap/bitstring, 1:1>>};
%  end;

%encode(MsgFields = [{FieldName1, _} | MsgFieldsRest], {_, FieldName2, _, _, optional, #copy{}},
%  Context#context{pmap = PMap}}) when FieldName1 =/= FieldName2 ->
%  {encode_string(null, true), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

%encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
%  Instr = {_, FieldName2, _, _, Presence, #copy{dictionary = D, key = Key, value = InitialValue}},
%     Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
%  Dict = select_dict(D, TemplateName, App),
%  case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
%     undef when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
%        {<<>>, MsgFieldsRest, Context#{pmap = <<PMap/bitstring, 0:1>>, dicts = Dicts1}};
%     undef when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_string(Value, Presence), MsgFieldsRest, Context#{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}};
%     Value when (FieldName1 == FieldName2)->
%        {<<>>, MsgFieldsRest, Context#{pmap = <<PMap/bitstring, 0:1>>}};
%     DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_string(Value, Presence), MsgFieldsRest, Context#{pmap = <<PMap/bitstring, 1:1>>, dicts = Dicts1}}
%  end.

%encode(MsgFields = [{FieldName1, _} | MsgFieldsRest], {_, FieldName2, _, _, optional, #delta{}}, Context)
%when FieldName1 =/= FieldName2 ->
%  {encode_string(null, true), MsgFields, Context};

%encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
%  Instr = {_, FieldName2, _, _, Presence, #delta{dictionary = D, key = Key, value = InitialValue}},
%     Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
%  Dict = select_dict(D, TemplateName, App),
%  case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
%     undef when (FieldName1 == FieldName2) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_delta(get_delta(Value, InitialValue), Presence), MsgFieldsRest, Context#{dicts = Dicts1}};
%     Value when (FieldName1 == FieldName2)->
%        {<<>>, MsgFieldsRest, Context};
%     DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_string(get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#{dicts = Dicts1}}
%  end.

%encode(MsgFields = [{FieldName1, _} | MsgFieldsRest], {_, FieldName2, _, _, optional, #tail{}},
%  Context#context{pmap = PMap}}) when FieldName1 =/= FieldName2 ->
%  {encode_string(null, true), MsgFields, Context#context{pmap = <<PMap/bitstring, 1:1>>}};

%encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
%  Instr = {_, FieldName2, _, _, Presence, #tail{dictionary = D, key = Key, value = InitialValue}},
%     Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
%  Dict = select_dict(D, TemplateName, App),
%  case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
%     undef when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
%        {encode_delta(get_delta(alue, InitialValue), Presence), MsgFieldsRest, Context#{dicts = Dicts1}};
%     undef when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_delta(get_delta(Value, InitialValue), Presence), MsgFieldsRest, Context#{dicts = Dicts1}};
%     Value when (FieldName1 == FieldName2)->
%        {<<>>, MsgFieldsRest, Context};
%     DictValue when (FieldName1 == FieldName2) andalso (Value =/= DictValue) ->
%        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
%        {encode_string(get_delta(Value, DictValue), Presence), MsgFieldsRest, Context#{dicts = Dicts1}}
%  end.
