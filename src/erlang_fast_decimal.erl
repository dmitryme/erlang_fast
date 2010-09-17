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
      ,select_dict/3
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
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, _, Data1} ->
         {{FieldName, absent}, Context#context{pmap = PMapRest}, Data1};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Context#context{pmap = PMapRest}, Data1}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, empty, Dicts),
         {{FieldName, absent}, Context#context{pmap = PMapRest, dicts = Dicts1}, Data1};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {{FieldName, Value}, Context#context{dicts = Dicts1, pmap = PMapRest}, Data1}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #copy{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty ->
         {{FieldName, empty}, Context};
      undef when (Presence == mandatory) and (InitialValue == undef) -> % ERR D5
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, empty}, Context#context{pmap = PMapRest, dicts = Dicts1}, Data};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{FieldName, InitialValue}, Context#context{dicts = Dicts1}, Data};
      Value ->
         {{FieldName, Value}, Context#context{pmap = PMapRest}, Data}
   end;

decode(Data, {_, FieldName, _, _, Presence, #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, _, Data1} ->
         {{FieldName, absent}, Context, Data1};
      {DecimalDelta = {MantissaDelta, ExponentDelta}, Err, Data1} ->
         L(Err, DecimalDelta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({'ERR D6', "Previous value is empty for delta operator"});
            undef when InitialValue == undef -> % initial base value is 0
               NewVal = {0 + MantissaDelta, 0 + ExponentDelta},
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#context{dicts = Dicts1}, Data1};
            undef ->
               NewVal = {element(1, InitialValue) + MantissaDelta, element(2, InitialValue) + ExponentDelta},
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#context{dicts = Dicts1}, Data1};
            {PrevMantissa, PrevExponent} ->
               NewVal = {PrevMantissa + MantissaDelta, PrevExponent + ExponentDelta},
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Context#context{dicts = Dicts1}, Data1}
         end
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context) ->
   case erlang_fast_number:decode(Data, #int32{name = FieldName, presence = Presence, operator = ExpOp}, Context) of
      R = {{FieldName, absent}, _, _} ->
         R;
      {{FieldName, Exponent}, Context1, Data1} ->
         {{FieldName, Mantissa}, Context2, Data2} =
            erlang_fast_number:decode(Data1, #int64{name = FieldName, presence = mandatory,
               operator = MantOp}, Context1),
         {{FieldName, {Mantissa, Exponent}}, Context2, Data2}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, undef}, Context = #context{logger = L}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      not_enough_data ->
         throw({error, [not_enough_data, Context]});
      {null, _, Data1} ->
         {{FieldName, absent}, Context, Data1};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Context, Data1}
   end;

decode(_, {_, _, _, _, _, _}, _) ->
   throw(invalid_decimal_type).
