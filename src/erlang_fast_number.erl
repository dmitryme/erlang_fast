-module(erlang_fast_number).


-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/3
      ,increment_value/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_uint/2
      ,decode_int/2
   ]).

decode(Data, {_, FieldName, _, _, Presence, #constant{value = InitialValue}},
   Context = #fast_context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
   when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
      case Presence of
         mandatory ->
            {{FieldName, InitialValue}, Context, Data};
         optional ->
            {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest}, Data}
      end;

decode(Data, {_, FieldName, _, _, optional, #constant{}},
   Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Context#fast_context{pmap = PMapRest}, Data};

decode(Data, {_, FieldName, _, _, _, #default{value = InitialValue}},
   Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   case InitialValue of
      undef ->
         {{FieldName, absent}, Context#fast_context{pmap = PMapRest}, Data};
      InitialValue ->
         {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest}, Data}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #default{value = _InitialValue}},
   Context = #fast_context{logger = L, pmap = <<1:1, PMapRest/bitstring>>}) ->
   Res = decode_number(Type, Data, is_nullable(Presence)),
   case Res of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, Data1} ->
         {{FieldName, absent}, Context#fast_context{pmap = PMapRest}, Data1};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Context#fast_context{pmap = PMapRest}, Data1}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
   Context = #fast_context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts}) ->
   Res = decode_number(Type, Data, is_nullable(Presence)),
   case Res of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Context#fast_context{pmap = PMapRest, dicts = Dicts1}, Data1};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {{FieldName, Value}, Context#fast_context{pmap = PMapRest, dicts = Dicts1}, Data1}
   end;

decode(Data, {_, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key, value = InitialValue}},
   Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts}) ->
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty ->
         {{FieldName, empty}, Context};
      undef when (Presence == mandatory) and (InitialValue == undef) -> % ERR D5
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, empty}, Context#fast_context{pmap = PMapRest, dicts = Dicts1}, Data};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{FieldName, InitialValue}, Context#fast_context{pmap = PMapRest, dicts = Dicts1}, Data};
      Value ->
         {{FieldName, Value}, Context#fast_context{pmap = PMapRest}, Data}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #increment{dictionary = Dict, key = Key}},
   Context = #fast_context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts}) ->
   case decode_number(Type, Data, is_nullable(Presence)) of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {Value, Err, DataRest} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {{FieldName, Value}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, DataRest}
   end;

decode(Data, {Type, FieldName, _, _, Presence, #increment{dictionary = Dict, key = Key, value = InitialValue}},
 Context = #fast_context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts}) ->
 case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
    empty ->
       {{FieldName, absent}, Context};
    undef when (InitialValue == undef) and (Presence == mandatory) -> % ERR D5
       throw({error, ['ERR D5', FieldName, "no initial value"]});
    undef when (InitialValue == undef) and (Presence == optional) -> % absent
       Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
       {{FieldName, absent}, Context#fast_context{dicts = Dicts1}, Data};
    undef ->
       Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
       {{FieldName, InitialValue}, Context#fast_context{dicts = Dicts1}, Data};
    Value ->
       NewValue = increment_value(Type, Value, 1),
       Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewValue, Dicts),
       {{FieldName, NewValue}, Context#fast_context{dicts = Dicts1, pmap = PMapRest}, Data}
 end;

decode(Data, {_, FieldName, _, _, Presence, #delta{dictionary = Dict, key = Key, value = InitialValue}},
   Context = #fast_context{logger = L, dicts = Dicts}) ->
   {NumDelta, Err, Data1} = decode_int(Data, is_nullable(Presence)),
   L(Err, NumDelta),
   case NumDelta of
      null ->
         {{FieldName, absent}, Context, Data1};
      NumDelta ->
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, ['ERR D6', "Previous value is empty for delta operator"]});
            undef when InitialValue == undef -> % initial base value is 0
               NewVal = 0 + NumDelta,
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, Data1};
            undef ->
               NewVal = InitialValue + NumDelta,
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, Data1};
            PrevValue ->
               NewVal = PrevValue + NumDelta,
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#fast_context{dicts = Dicts1}, Data1}
         end
   end;

decode(Data, {Type, FieldName, _, _, Presence, undef}, Context = #fast_context{logger = L}) ->
  {Value, Err, DataRest} = decode_number(Type, Data, is_nullable(Presence)),
  L(Err, Value),
  case Value of
     null ->
        {{FieldName, absent}, Context, DataRest};
     Value ->
        {{FieldName, Value}, Context, DataRest}
  end;

decode(_, _, _) ->
   throw({error, [invalid_number_type]}).

%% ==========================================================================================
%% helpers
%% ==========================================================================================

decode_number(Type, Data, Nullable) when (Type == uInt32) or (Type == uInt64) ->
   decode_uint(Data, Nullable);
decode_number(Type, Data, Nullable) when (Type == int32) or (Type == int64) ->
   decode_int(Data, Nullable).