%decimal decoding

-module(erlang_fast_decimal).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_scaled/2
   ]).

decode(Data, {decimal, FieldName, _, _, Presence, #constant{value = InitialValue}},
  Context = #context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
  when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
     case Presence of
        mandatory ->
           {{FieldName, InitialValue}, Context, Data};
        optional ->
           {{FieldName, InitialValue}, Context#context{pmap = PMapRest}, Data}
     end;

decode(Data, {decimal, FieldName, _, _, optional, #constant{}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Context#context{pmap = PMapRest}, Data};

decode(Data, {decimal, FieldName, _, _, optional, #default{value = InitialValue}},
    Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
 case InitialValue of
    undef ->
       {{FieldName, absent}, Context#context{pmap = PMapRest}, Data};
    InitialValue ->
       {{FieldName, InitialValue}, Context#context{pmap = PMapRest}, Data}
 end;

decode(Data, {decimal, FieldName, _, _, Presence, #default{value = _InitialValue}},
  Context = #context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>})
  when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
     Res = decode_scaled(Data, is_nullable(Presence)),
     case Res of
        not_enough_data ->
           throw({not_enough_data, Context});
        {Value, Err, DataRest} ->
           L(Err, Value),
           case Presence of
              mandatory ->
                 {{FieldName, Value}, Context, DataRest};
              optional ->
                 {{FieldName, Value}, Context#context{pmap = PMapRest}, DataRest}
           end
     end;

decode(Data, {decimal, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
    Context = #context{logger = L, pmap = <<PresenceBit:1, PMapRest/bitstring>>, dicts = Dicts})
 when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1)->
    Res = decode_scaled(Data, is_nullable(Presence)),
    case Res of
       not_enough_data ->
          throw({not_enough_data, Context});
       {Value, Err, DataRest} ->
          L(Err, Value),
          case Presence of
             mandatory ->
                Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
                {{FieldName, Value}, Context#context{dicts = Dicts1}, DataRest};
             optional ->
                Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
                {{FieldName, Value}, Context#context{dicts = Dicts1, pmap = PMapRest}, DataRest}
          end
    end;

decode(Data, {decimal, FieldName, _, _, optional, #copy{dictionary = Dict, key = Key, value = InitialValue}},
 Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts}) ->
 case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
    empty ->
       {{FieldName, empty}, Context};
    undef when InitialValue == undef -> % it becomes empty
       Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
       {{FieldName, empty}, Context#context{dicts = Dicts1}, Data};
    undef ->
       Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
       {{FieldName, InitialValue}, Context#context{dicts = Dicts1}, Data};
    Value ->
       {{FieldName, Value}, Context#context{pmap = PMapRest}, Data}
 end;

decode(Data, {_, FieldName, _, _, Presence, #delta{dictionary = Dict, key = Key, value = InitialValue}},
  Context = #context{logger = L, dicts = Dicts}) ->
  {DecimalDelta, Err, DataRest} = decode_scaled(Data, is_nullable(Presence)),
  L(Err, DecimalDelta),
  case DecimalDelta of
     null ->
        {{FieldName, absent}, Context, DataRest};
     {MantissaDelta, ExponentDelta} ->
        case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
           empty ->
              throw({'ERR D6', "Previous value is empty for delta operator"});
           undef when InitialValue == undef -> % initial base value is 0
              NewVal = {0 + MantissaDelta, 0 + ExponentDelta},
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#context{dicts = Dicts1}, DataRest};
           undef ->
              NewVal = {element(1, InitialValue) + MantissaDelta, element(2, InitialValue) + ExponentDelta},
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#context{dicts = Dicts1}, DataRest};
           {PrevMantissa, PrevExponent} ->
              NewVal = {PrevMantissa + MantissaDelta, PrevExponent + ExponentDelta},
              Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
              {{FieldName, NewVal}, Context#context{dicts = Dicts1}, DataRest}
        end
  end;

decode(Data, {decimal, FieldName, _, _, Presence, #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context) ->
   case erlang_fast_number:decode(Data, #int32{name = FieldName, presence = Presence, operator =
            ExpOp}, Context) of
      R = {{FieldName, absent}, _, _} ->
         R;
      {{FieldName, Exponent}, Context1, Data1} ->
         {{FieldName, Mantissa}, Context2, Data2} =
            erlang_fast_number:decode(Data1, #int64{name = FieldName, presence = mandatory,
               operator = MantOp}, Context1),
         {{FieldName, {Mantissa, Exponent}}, Context2, Data2}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, undef}, Context = #context{logger = L}) ->
   {Value, Err, DataRest} = decode_scaled(Data, is_nullable(Presence)),
   L(Err, Value),
   case Value of
      null ->
         {{FieldName, absent}, Context, DataRest};
      Value ->
         {{FieldName, Value}, Context, DataRest}
   end;

decode(_, {_, _, _, _, _, _}, _) ->
   throw(invalid_decimal_type).
