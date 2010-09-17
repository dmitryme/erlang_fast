%string decoding

-module(erlang_fast_string).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/4
      ,select_dict/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_string/2
      ,decode_string_delta/2
      ,decode_vector/2
      ,decode_vector_delta/2
   ]).

decode(Data, {_, FieldName, _, _, Presence, {constant, InitialValue}},
   Context = #context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
   when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
      case Presence of
         mandatory ->
            {{FieldName, InitialValue}, Data, Context};
         optional ->
            {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
      end;

decode(Data, {_, FieldName, _, _, optional, {constant, _}},
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

decode(Data, {Type, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
   end;

decode(Data, {_Type, FieldName, _, _, Presence, #copy{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty ->
         {{FieldName, empty}, Data, Context#context{pmap = PMapRest}};
      undef when (Presence == mandatory) and (InitialValue == undef) -> %ERR D5
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, empty}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
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
      {StringDelta = {Len, String}, Err, Data1} ->
         L(Err, StringDelta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, ['ERR D6', "Previous value is empty for delta operator"]});
            undef when InitialValue == undef -> % initial base value is "" or <<>>
               NewVal = apply_delta(Type, initial_base_value(Type), Len, String),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            undef ->
               NewVal = apply_delta(Type, InitialValue, Len, String),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(Type, PrevValue, Len, String),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}}
         end
   end;

decode(Data, {Type, FieldName, _, _, Presence, #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, pmap = <<PresenceBit:1, PMapRest/bitstring>>,
      application = App, template = #template{name = TemplateName}})
      when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_string(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} when (Presence == optional) andalso (PresenceBit == 1) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}};
      {StringTail, Err, Data1} ->
         L(Err, StringTail),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            PrevValue when ((PrevValue == undef) or (PrevValue == empty)) andalso (InitialValue == undef) -> % default base value is ""
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, StringTail, Dicts),
               {{FieldName, StringTail}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}};
            PrevValue when ((PrevValue == undef) or (PrevValue == empty)) -> % base valie is a initial value
               NewVal = string:join([InitialValue, StringTail], []),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}};
            PrevValue ->
               NewVal = string:join([PrevValue, StringTail], []),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}}
         end
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

initial_base_value(string) -> "";
initial_base_value(Type) when (Type == unicode) or (Type == byteVector) -> <<>>.
