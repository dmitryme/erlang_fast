-module(erlang_fast_field_decode).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/2
      ,increment_value/3
      ,select_dict/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_type/3
      ,decode_delta/3
   ]).

%% =========================================================================================================
%% field decoding
%% =========================================================================================================

decode(Data, Instr, Context = #context{pmap = <<>>}) ->
   decode(Data, Instr, Context#context{pmap = <<0:1>>});

%% =========================================================================================================
%% templateRef
%% =========================================================================================================

decode(Data, #templateRef{name = undef}, Context) ->
   {Msg, Data1, #context{dicts = D}} = erlang_fast_segment:decode(Data, Context),
   {Msg, Data1, Context#context{dicts = D}};

decode(Data, #templateRef{name = Name}, Context = #context{template = T = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(Name, Context#context.templates#templates.tlist),
   {skip, Data, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

%% =========================================================================================================
%% templateRef
%% =========================================================================================================

decode(Data, #typeRef{name = TypeName}, Context) ->
   {skip, Data, Context#context{application = TypeName}};

%% =========================================================================================================
%% constant
%% =========================================================================================================

decode(Data, #field{name = FieldName, presence = Presence, operator = #constant{value = InitialValue}},
   Context = #context{pmap = <<PresenceBit:1, PMapRest/bitstring>>}) ->
   case Presence of
      mandatory ->
         {{FieldName, InitialValue}, Data, Context};
      optional when PresenceBit == 1 ->
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}};
      optional when PresenceBit == 0 ->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}}
   end;

%% =========================================================================================================
%% default
%% =========================================================================================================

decode(Data, #field{name = FieldName, operator = #default{value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   case InitialValue of
      undef ->
         {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};
      InitialValue ->
         {{FieldName, InitialValue}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #default{value = _InitialValue}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest}};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest}}
   end;

%% =========================================================================================================
%% copy
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #copy{dictionary = D, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {{FieldName, Value}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
   end;

decode(Data, #field{name = FieldName, presence = Presence, operator = #copy{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
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

%% =========================================================================================================
%% increment
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #increment{dictionary = Dict, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bitstring>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {{FieldName, Value}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #increment{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = <<0:1, PMapRest/bitstring>>, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, ['ERR D6', FieldName, "Previous value is empty for mandatory field"]});
      empty when (Presence == optional)->
         {{FieldName, absent}, Data, Context};
      undef when (InitialValue == undef) and (Presence == mandatory) -> % ERR D5
         throw({error, ['ERR D5', FieldName, "no initial value"]});
      undef when (InitialValue == undef) and (Presence == optional) -> % absent
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{FieldName, absent}, Data, Context#context{dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{FieldName, InitialValue}, Data, Context#context{dicts = Dicts1}};
      Value ->
         NewValue = increment_value(Type, Value, 1),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewValue, Dicts),
         {{FieldName, NewValue}, Data, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

%% =========================================================================================================
%% delta
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   case decode_delta(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, ['ERR D6', FieldName, "Previous value is empty for delta operator"]});
            undef ->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{FieldName, NewVal}, Data1, Context#context{dicts = Dicts1}}
         end
   end;

%% =========================================================================================================
%% tail
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, pmap = <<1:1, PMapRest/bitstring>>,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
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

decode(Data, #field{name = FieldName, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
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

%% =========================================================================================================
%% decFieldOp
%% =========================================================================================================

decode(Data, #field{name = FieldName, presence = Presence, operator = #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context) ->
   case decode(Data, #field{type = int32, name = FieldName, presence = Presence, operator = ExpOp}, Context) of
      R = {{FieldName, absent}, _, _} ->
         R;
      {{FieldName, Exponent}, Data1, Context1} ->
         {{FieldName, Mantissa}, Data2, Context2} =
            decode(Data1, #field{type = int64, name = FieldName, presence = mandatory, operator = MantOp}, Context1),
         {{FieldName, {Mantissa, Exponent}}, Data2, Context2}
   end;

%% =========================================================================================================
%% no operator
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, presence = Presence, operator = undef}, Context = #context{logger = L}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{FieldName, absent}, Data1, Context};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{FieldName, Value}, Data1, Context}
   end;

%% =========================================================================================================
%% group decoding
%% =========================================================================================================

decode(Data, #field_group{type = group, name = FieldName}, Context = #context{pmap = <<0:1, PMapRest/bitstring>>}) ->
   {{FieldName, absent}, Data, Context#context{pmap = PMapRest}};

decode(Data, #field_group{type = group, name = FieldName, need_pmap = NeedPMap, instructions = Instrs},
   Context = #context{pmap = <<1:1, PMapRest/bitstring>>}) ->
   {Data1, Context1} =
   case NeedPMap of
      true ->
         erlang_fast_segment:decode_pmap(Data, Context);
      false ->
         {Data, Context}
   end,
   {Msg, Data2, #context{dicts = Dicts}} =
      erlang_fast_segment:decode_fields(Data1, Context1#context.template#template{instructions = Instrs}),
   {{FieldName, Msg}, Data2, Context#context{pmap = PMapRest, dicts = Dicts}};

%% =========================================================================================================
%% sequence decoding
%% =========================================================================================================
decode(Data, #field_group{type = sequence, name = FieldName, instructions = []}, Context) ->
   {{FieldName, absent}, Data, Context};

decode(Data, #field_group{type = sequence, name = FieldName, presence = Presence, need_pmap = NeedPMap, instructions = Instructions},
   Context = #context{pmap = <<_:1, PMapRest/bitstring>>}) ->
   LenField = case (hd(Instructions))#field.type == length of
      true ->
         LenInstr = hd(Instructions),
         LenInstr#field{type = uInt32, presence = Presence};
      false ->
         #field{type = uInt32, name = "length", presence = Presence}
   end,
   {{_, LenValue}, Data1, Context1} = decode(Data, LenField, Context),
   case LenValue of
      absent ->
         {{FieldName, absent}, Data1, Context1#context{pmap = PMapRest}};
      LenValue ->
         Instrs = case (hd(Instructions))#field.type == length of
            true ->
               tl(Instructions);
            false ->
               Instructions
         end,
         {Sequence, Data2, #context{dicts = Dicts}} = decode_sequence_aux(LenValue, Data1, NeedPMap,
            Context1#context{pmap = PMapRest, template = Context1#context.template#template{instructions = Instrs}}),
         {{FieldName, Sequence}, Data2, Context#context{pmap = PMapRest, dicts = Dicts}}
   end;

%% =========================================================================================================
%% decoding terminator
%% =========================================================================================================
decode(_, Instr, _) ->
   throw({error, [unknown_field, Instr]}).


%% =========================================================================================================
%% decoding terminator
%% =========================================================================================================
decode_sequence_aux(0, Data, _NeedPMap, Context) ->
   {[], Data, Context};

decode_sequence_aux(Length, Data, NeedPMap, Context = #context{template = Template}) ->
   case NeedPMap of
      true ->
         {Data1, Context1} = erlang_fast_segment:decode_pmap(Data, Context),
         {Msg, Data2, Context2} = erlang_fast_segment:decode_fields(Data1, Context1),
         {Msgs, Data3, Context3} = decode_sequence_aux(Length - 1, Data2, NeedPMap, Context2#context{template = Template}),
         {[Msg | Msgs], Data3, Context3};
      false ->
         {Msg, Data1, Context1} = erlang_fast_segment:decode_fields(Data, Context),
         {Msgs, Data2, Context2} = decode_sequence_aux(Length - 1, Data1, NeedPMap, Context1#context{template = Template}),
         {[Msg | Msgs], Data2, Context2}
   end.
