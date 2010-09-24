%decimal decoding

-module(erlang_fast_decimal).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/2
      ,select_dict/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_scaled/2
   ]).

%% =========================================================================================================
%% decoding
%% =========================================================================================================

decode(Data, {decimal, FieldName, _, _, Presence, #constant{value = InitialValue}},
   Context = #context{pmap = <<PresenceBit:1, PMapRest/bitstring>>})
when (Presence == mandatory) or (Presence == optional andalso PresenceBit == 1) ->
   case Presence of
      mandatory ->
         {{FieldName, InitialValue}, Data, Context};
      optional ->
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {decimal, FieldName, _, _, optional, #constant{}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};

decode(Data, {decimal, FieldName, _, _, _, #default{value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   case InitialValue of
      undef ->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};
      InitialValue ->
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #default{value = _InitialValue}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest}};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest}}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #copy{dictionary = Dict, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {{FieldName, Value}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #copy{dictionary = D, key = Key, value = InitialValue}},
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
         {{FieldName, InitialValue}, Data, Context#context{dicts = Dicts1}};
      Value ->
         {{FieldName, Value}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, {_, FieldName, _, _, Presence, #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {DecimalDelta, Err, Data1} ->
         L(Err, DecimalDelta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({'ERR D6', FieldName, "Previous value is empty for delta operator"});
            undef ->
               NewVal = apply_delta(InitialValue, DecimalDelta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            PrevDecimal ->
               NewVal = apply_delta(PrevDecimal, DecimalDelta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}}
         end
   end;

decode(Data, {decimal, FieldName, _, _, Presence, #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context) ->
   case erlang_fast_number:decode(Data, #int32{name = FieldName, presence = Presence, operator = ExpOp}, Context) of
      R = {{FieldName, absent}, _, _} ->
         R;
      {{FieldName, Exponent}, Data1, Context1} ->
         {{FieldName, Mantissa}, Data2, Context2} =
         erlang_fast_number:decode(Data1, #int64{name = FieldName, presence = mandatory,
               operator = MantOp}, Context1),
         {{FieldName, {Mantissa, Exponent}}, Data2, Context2}
   end;

decode(Data, {decimal, FieldName, _, _, Presence, undef}, Context = #context{logger = L}) ->
   case decode_scaled(Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context}
   end;

decode(_, Instr, _) ->
   throw({error, [unknown_decimal_type, Instr]}).
