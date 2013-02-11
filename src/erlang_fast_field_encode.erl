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
      ,encode_pmap/1
   ]).

%% =========================================================================================================
%% encoding
%% =========================================================================================================

%% =========================================================================================================
%% constant
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{name = FieldName2, presence = Presence, operator = #constant{}},
      Context = #context{pmap = PMap}) ->
   case Presence of
      mandatory ->
         {<<>>, MsgFieldsRest, Context};
      optional when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
         {<<>>, MsgFields, Context#context{pmap = <<PMap/bits, 0:1>>}};
      optional when FieldName1 == FieldName2 ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}}
   end;

%% =========================================================================================================
%% default
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = #default{value = InitialValue}},
      Context = #context{pmap = PMap}) ->
   case Presence of
      optional when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
         {<<>>, MsgFields, Context#context{pmap = <<PMap/bits, 0:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value =/= InitialValue) ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};
      _ when (FieldName1 == FieldName2) andalso (Value == InitialValue) ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}}
   end;

%% =========================================================================================================
%% copy
%% =========================================================================================================

encode([{FieldName1, Value} | _],#field{name = FieldName2, presence = mandatory, operator =
      #copy{}}, _Context) when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   throw({error, {'ERR D6', FieldName2, "Mandatory field can not be absent with copy operator."}});

encode(MsgFields = [{FieldName1, Value} | _], #field{type = Type, name = FieldName2, presence = optional, operator = #copy{}},
      Context = #context{pmap = PMap})
   when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   {encode_type(Type, null, true), MsgFields, Context#context{pmap = <<PMap/bits, 1:1>>}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #copy{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef when (Value == InitialValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>, dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>, dicts = Dicts1}}
   end;

%% =========================================================================================================
%% delta
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | _], #field{type = Type, name = FieldName2, presence = Presence, operator = #delta{}}, Context)
when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFields, Context};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), is_nullable(Presence)), MsgFieldsRest, Context#context{dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), is_nullable(Presence)), MsgFieldsRest, Context#context{dicts = Dicts1}}
   end;

%% =========================================================================================================
%% tail
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | _], #field{type = Type, presence = Presence, name = FieldName2, operator = #tail{}},
   Context = #context{pmap = PMap})
when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFields, Context#context{pmap = <<PMap/bits, 0:1>>}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), is_nullable(Presence)), MsgFieldsRest,
            Context#context{pmap = <<PMap/bits, 1:1>>, dicts = Dicts1}};
      Value ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};
      DictValue when (Value =/= DictValue) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), is_nullable(Presence)), MsgFieldsRest, Context#context{pmap =
               <<PMap/bits, 1:1>>, dicts = Dicts1}}
   end;

%% =========================================================================================================
%% increment
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | _], #field{type = Type, presence = Presence, name = FieldName2, operator =
      #increment{}}, Context = #context{pmap = PMap})
when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFields, Context#context{pmap = <<PMap/bits, 1:1>>}};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #increment{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef when (InitialValue == Value) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>, dicts = Dicts1}};
      undef when (InitialValue == undef) orelse (InitialValue =/= Value) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>,
            dicts = Dicts1}};
      DictValue when (Value - DictValue == 1) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>, dicts = Dicts1}};
      _ ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>,
         dicts = Dicts1}}
   end;

%% =========================================================================================================
%% undef
%% =========================================================================================================

encode(MsgFields = [{FieldName1, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName2, presence = Presence, operator = undef}, Context) ->
   case (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) of
      true ->
         {encode_type(Type, null, true), MsgFields, Context};
      false ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context}
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
      Context#context{pmap = <<PMap/bits, (Context1#context.pmap)/bits>>, dicts = Context1#context.dicts}};

encode(MsgFields, #templateRef{name = TemplateName}, Context = #context{template = T = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(TemplateName, Context#context.templates#templates.tlist),
   {<<>>, MsgFields, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

%% =========================================================================================================
%% decFieldOp
%% =========================================================================================================
encode(MsgFields = [{FieldName1, Value} | _],
   #field{name = FieldName2, presence = Presence, operator = #decFieldOp{}}, Context = #context{pmap = PMap})
when (FieldName1 =/= FieldName2) orelse ((FieldName1 == FieldName2) andalso (Value == absent)) ->
   {encode_type(decimal, null, is_nullable(Presence)), MsgFields, Context#context{pmap = <<PMap/bits, 0:1>>}};

encode([{FieldName, {Mantissa, Exponent}} | MsgFieldsRest],
   F = #field{name = FieldName, presence = mandatory, operator = #decFieldOp{exponent = EOp, mantissa = MOp}}, Context) ->
   {EBin, [], Context1} = encode([{FieldName, Exponent}], F#field{type = int32, operator = EOp}, Context),
   {MBin, [], Context2} = encode([{FieldName, Mantissa}], F#field{type = int64, operator = MOp}, Context1),
   {<<EBin/bits, MBin/bits>>, MsgFieldsRest, Context2};

%% =========================================================================================================
%% group
%% =========================================================================================================
encode(MsgFields = [{GroupName1, _} | _], #field_group{type = group, name = GroupName2}, Context = #context{pmap = PMap})
when GroupName1 =/= GroupName2 ->
   {<<>>, MsgFields, Context#context{pmap = <<PMap/bits, 0:1>>}};

encode([{GroupName, MsgGroupFields} | MsgFieldsRest],
   #field_group{type = group, name = GroupName, instructions = Instrs}, Context = #context{pmap = PMap, template = T}) ->
   {BinGroupFields, [], #context{pmap = GroupPMap}} = erlang_fast_segment:encode_fields(
      MsgGroupFields, Context#context{pmap = <<>>, template = T#template{instructions = Instrs}}),
   {<<GroupPMap/bits, BinGroupFields/bits>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};

%% =========================================================================================================
%% sequence
%% =========================================================================================================
encode(MsgFields = [{SeqName1, MsgSeqFields} | _],
  #field_group{type = sequence, name = SeqName2, presence = Presence, instructions = Instrs}, Context = #context{pmap = PMap})
when (SeqName1 =/= SeqName2) or ((SeqName1 == SeqName2) andalso (length(MsgSeqFields) == 0)) ->
  LenField =
  case (hd(Instrs))#field.type == length of
     true ->
        LenInstr = hd(Instrs),
        LenInstr#field{type = uInt32, presence = Presence};
     false ->
        #field{type = uInt32, name = "length", presence = Presence}
  end,
  {LenBinary, _, #context{pmap = LenPMap}} = encode([{LenField#field.name, absent}], LenField, Context#context{pmap = <<>>}),
  {LenBinary, MsgFields, Context#context{pmap = <<PMap/bits, LenPMap>>}};

encode([{SeqName, MsgSeqFields} | MsgFieldsRest],
   #field_group{type = sequence, name = SeqName, presence = Presence, instructions = Instrs},
   Context = #context{template = T}) ->
   {LenField, SeqInstrs} =
   case (hd(Instrs))#field.type == length of
      true ->
         LenInstr = hd(Instrs),
         {LenInstr#field{type = uInt32, presence = Presence}, tl(Instrs)};
      false ->
         {#field{type = uInt32, name = "length", presence = Presence}, Instrs}
   end,
   {LenBin, _, Context1} =
      encode([{LenField#field.name, length(MsgSeqFields)}], LenField, Context),
   {SeqBin, #context{dicts = Dicts1}} =
      encode_seq_aux(MsgSeqFields, Context1#context{pmap = <<>>, template = T#template{instructions = SeqInstrs}}),
   {<<LenBin/bits, SeqBin/bits>>, MsgFieldsRest, Context1#context{dicts = Dicts1}};

%% =========================================================================================================
%% terminator
%% =========================================================================================================
encode(_, Instr, _Context) ->
   throw({error, {unknown_field, Instr}}).

%% =========================================================================================================
%% encode sequence fields
%% =========================================================================================================
encode_seq_aux([], Context) ->
   {<<>>, Context};
encode_seq_aux([MsgFields | MsgFieldsRest], Context) ->
   {Head, [], #context{pmap = HeadPMap, dicts = Dicts}} = erlang_fast_segment:encode_fields(MsgFields, Context),
   {TailBin, Context1} = encode_seq_aux(MsgFieldsRest, Context#context{pmap = <<>>, dicts = Dicts}),
   {<<(encode_pmap(HeadPMap))/bits, Head/bits, TailBin/bits>>, Context1}.
