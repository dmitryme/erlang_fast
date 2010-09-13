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
   ]).

-import(erlang_fast_decode_types,
   [
      decode_string/2
      ,decode_string_delta/2
      ,decode_vector/2
      ,decode_vector_delta/2
   ]).

decode(Data, {_, FieldName, _, _, Presence, {constant, InitialValue}},
   Context = #fast_context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
   when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
      case Presence of
         mandatory ->
            {{FieldName, InitialValue}, Context, Data};
         optional ->
            {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest}, Data}
      end;

decode(Data, {_, FieldName, _, _, optional, {constant, _}},
   Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Context#fast_context{pmap = PMapRest}, Data};

decode(Data, {_, FieldName, _, _, optional, #default{value = InitialValue}},
      Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   case InitialValue of
      undef ->
         {{FieldName, absent}, Context#fast_context{pmap = PMapRest}, Data};
      InitialValue ->
         {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest}, Data}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #default{value = _InitialValue}},
   Context = #fast_context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>})
when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
   Res = decode_string(Type, Data, is_nullable(Presence)),
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

decode(Data, {Type, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
   Context = #fast_context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>, dicts = Dicts})
when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1)->
   Res = decode_string(Type, Data, is_nullable(Presence)),
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

decode(Data, {_Type, FieldName, _, _, optional, #copy{dictionary = Dict, key = Key, value = InitialValue}},
  Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts}) ->
  case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
     empty ->
        {{FieldName, empty}, Context};
     undef when InitialValue == undef -> % it becomes empty
        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
        {{FieldName, empty}, Context#fast_context{dicts = Dicts1}, Data};
     undef ->
        Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
        {{FieldName, InitialValue}, Context#fast_context{dicts = Dicts1}, Data};
     Value ->
        {{FieldName, Value}, Context#fast_context{pmap = PMapRest}, Data}
  end;

decode(Data, {Type, FieldName, _, _, Presence, #delta{dictionary = Dict, key = Key, value = InitialValue}},
  Context = #fast_context{logger = L, dicts = Dicts}) ->
  {StringDelta, Err, DataRest} = decode_string_delta(Type, Data, is_nullable(Presence)),
  L(Err, StringDelta),
  case StringDelta of
     null ->
        {{FieldName, absent}, Context, DataRest};
     {Len, String} ->
        case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
           empty ->
              throw({'ERR D6', "Previous value is empty for delta operator"});
           undef when InitialValue == undef -> % initial base value is "" or <<>>
              NewVal = apply_delta(Type, initial_base_value(Type), Len, String),
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, DataRest};
           undef ->
              NewVal = apply_delta(Type, InitialValue, Len, String),
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, DataRest};
           PrevValue ->
              NewVal = apply_delta(Type, PrevValue, Len, String),
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, DataRest}
        end
  end;

decode(Data, {Type, FieldName, _, _, Presence, #tail{dictionary = Dict, key = Key, value = InitialValue}},
   Context = #fast_context{logger = L, dicts = Dicts, pmap = <<PresenceBit:1, PMapRest/bitstring>>})
when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
   {StringTail, Err, DataRest} = decode_string(Type, Data, is_nullable(Presence)),
   L(Err, StringTail),
   case StringTail of
      null when (Presence == optional) andalso (PresenceBit == 1) ->
         Dicts1 = erlang_fast_dict:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest};
      StringTail ->
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            PrevValue when ((PrevValue == undef) or (PrevValue == empty)) andalso (InitialValue == undef) -> % default base value is ""
               Dicts1 = erlang_fast_dict:put_value(Dict, Key, StringTail, Dicts),
               {{FieldName, StringTail}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest};
            PrevValue when ((PrevValue == undef) or (PrevValue == empty)) -> % base valie is a initial value
               NewVal = string:join([InitialValue, StringTail], []),
               Dicts1 = erlang_fast_dict:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest};
            PrevValue ->
               NewVal = string:join([PrevValue, StringTail], []),
               Dicts1 = erlang_fast_dict:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest}
         end
   end;

decode(Data, {Type, FieldName, _, _, Presence, undef}, Context = #fast_context{logger = L}) ->
   {Value, Err, DataRest} = decode_string(Type, Data, is_nullable(Presence)),
   L(Err, Value),
   case Value of
      null ->
         {{FieldName, absent}, Context, DataRest};
      Value ->
         {{FieldName, Value}, Context, DataRest}
   end;

decode(_, _, _) ->
   throw(invalid_string_type).

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
