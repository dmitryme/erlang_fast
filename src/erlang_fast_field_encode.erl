-module(erlang_fast_field_encode).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

encode([], F = #field{name = FieldName}, Context) ->
   encode([{FieldName, absent}], F, Context);

encode(MsgFields = [{FieldName1, _Value} | _], F = #field{name = FieldName2}, Context) when (FieldName1 =/= FieldName2) ->
   encode([{FieldName2, absent} | MsgFields], F, Context);

encode([], F = #field_group{name = GroupName, type = group}, Context) ->
   encode([{GroupName, absent}], F, Context);

encode(MsgFields = [{GroupName1, _Value} | _], F = #field_group{name = GroupName2, type = group}, Context) when (GroupName1 =/= GroupName2) ->
   encode([{GroupName2, absent} | MsgFields], F, Context);

encode([], F = #field_group{name = GroupName, type = sequence}, Context) ->
   encode([{GroupName, []}], F, Context);

encode(MsgFields = [{GroupName1, _Value} | _], F = #field_group{name = GroupName2, type = sequence}, Context) when (GroupName1 =/= GroupName2) ->
   encode([{GroupName2, []} | MsgFields], F, Context);

%% =========================================================================================================
%% constant
%% =========================================================================================================

encode([{FieldName, Value} | MsgFieldsRest], #field{name = FieldName, presence = Presence, operator =
      #constant{value = InitialValue}}, Context = #context{pmap = PMap}) ->
   case Presence of
      mandatory when Value == absent ->
         throw({error, {'ERR D3', FieldName, "Mandatory constant field not found."}});
      mandatory when Value =/= InitialValue ->
         throw({error, {'ERR D3', FieldName, "Constant initial value is not the same as field one."}});
      mandatory ->
         {<<>>, MsgFieldsRest, Context};
      optional when Value == absent ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};
      optional ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}}
   end;

%% =========================================================================================================
%% default
%% =========================================================================================================

encode([{FieldName, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName, presence = Presence, operator = #default{value = InitialValue}},
      Context = #context{pmap = PMap}) ->
   case Presence of
      optional when (Value == absent) andalso (InitialValue == undef) ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};
      optional when (Value == absent) ->
         {encode_type(Type, null, true), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};
      _ when (Value =/= InitialValue) ->
         {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};
      _ when (Value == InitialValue) ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}}
   end;

%% =========================================================================================================
%% copy
%% =========================================================================================================

encode([{FieldName, Value} | _], #field{name = FieldName, presence = mandatory, operator = #copy{}}, _Context)
   when (Value == absent) ->
   throw({error, {'ERR D6', FieldName, "Mandatory field can not be absent with copy operator."}});

encode([{FieldName, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName, presence = optional, operator = #copy{dictionary = D, key = Key}},
      Context = #context{pmap = PMap, dicts = Dicts, application = App, template = #template{name = TemplateName}})
   when (Value == absent) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty ->
         {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};
      _Value -> % or undef
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {encode_type(Type, null, true), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>, dicts = Dicts1}}
   end;

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
      empty ->
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

encode([{FieldName, Value} | MsgFieldsRest], #field{type = Type, name = FieldName, presence = Presence, operator = #delta{}}, Context)
when (Value == absent) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFieldsRest, Context};

encode([{_, Value} | MsgFieldsRest],
   #field{type = Type, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, InitialValue), is_nullable(Presence)), MsgFieldsRest, Context#context{dicts = Dicts1}};
      DictValue ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {encode_delta(Type, get_delta(Value, DictValue), is_nullable(Presence)), MsgFieldsRest, Context#context{dicts = Dicts1}}
   end;

%% =========================================================================================================
%% tail
%% =========================================================================================================

encode([{FieldName, Value} | MsgFieldsRest], #field{type = Type, presence = Presence, name = FieldName, operator = #tail{}},
   Context = #context{pmap = PMap}) when (Value == absent) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};

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

encode([{FieldName, Value} | MsgFieldsRest], #field{type = Type, presence = Presence, name = FieldName, operator =
      #increment{}}, Context = #context{pmap = PMap}) when (Value == absent) ->
   {encode_type(Type, null, is_nullable(Presence)), MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};

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
%% no operator
%% =========================================================================================================

encode([{FieldName, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName, presence = _Presence, operator = undef}, Context) when (Value == absent) ->
   {encode_type(Type, null, true), MsgFieldsRest, Context};

encode([{FieldName, Value} | MsgFieldsRest],
   #field{type = Type, name = FieldName, presence = Presence, operator = undef}, Context) ->
   {encode_type(Type, Value, is_nullable(Presence)), MsgFieldsRest, Context};

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
   {ok, {TemplRefBin, _, Context1}} = erlang_fast_segment:encode(MsgRefFields, Context#context{template = TemplateRef}),
   {TemplRefBin, MsgFieldsRest,
      Context#context{pmap = <<PMap/bits, (Context1#context.pmap)/bits>>, dicts = Context1#context.dicts}};

encode(MsgFields, #templateRef{name = TemplateName}, Context = #context{template = T = #template{instructions = Instrs}}) ->
   TemplateRef = erlang_fast_templates:get_by_name(TemplateName, Context#context.templates#templates.tlist),
   {<<>>, MsgFields, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

%% =========================================================================================================
%% decFieldOp
%% =========================================================================================================
encode(MsgFields = [{FieldName, Value} | MsgFieldsRest],
   F = #field{name = FieldName, presence = Presence, operator = #decFieldOp{exponent = EOp}}, Context)
when (Value == absent) ->
   {EBin, [], Context1} = encode(MsgFields, F#field{type = int32, name = FieldName, presence = Presence, operator = EOp}, Context),
   {EBin, MsgFieldsRest, Context1};

encode([{FieldName, {Mantissa, Exponent}} | MsgFieldsRest],
   F = #field{name = FieldName, presence = Presence, operator = #decFieldOp{exponent = EOp, mantissa = MOp}}, Context) ->
   {EBin, [], Context1} = encode([{FieldName, Exponent}], F#field{type = int32, presence = Presence, operator = EOp}, Context),
   {MBin, [], Context2} = encode([{FieldName, Mantissa}], F#field{type = int64, presence = mandatory, operator = MOp}, Context1),
   {<<EBin/bits, MBin/bits>>, MsgFieldsRest, Context2};

%% =========================================================================================================
%% group
%% =========================================================================================================
encode([{GroupName, Value} | MsgFieldsRest], #field_group{type = group, name = GroupName}, Context = #context{pmap = PMap})
when  (Value == absent) ->
   {<<>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 0:1>>}};

encode([{GroupName, MsgGroupFields} | MsgFieldsRest],
   #field_group{type = group, name = GroupName, instructions = Instrs}, Context = #context{pmap = PMap, template = T}) ->
   {BinGroupFields, [], #context{pmap = GroupPMap}} = erlang_fast_segment:encode_fields(
      MsgGroupFields, Context#context{pmap = <<>>, template = T#template{instructions = Instrs}}),
   {<<GroupPMap/bits, BinGroupFields/bits>>, MsgFieldsRest, Context#context{pmap = <<PMap/bits, 1:1>>}};

%% =========================================================================================================
%% sequence
%% =========================================================================================================
encode([{SeqName, MsgSeqFields} | MsgFieldsRest],
  #field_group{type = sequence, name = SeqName, instructions = [LenField | _Instrs]},
  Context = #context{pmap = PMap})
when (length(MsgSeqFields) == 0) ->
  {LenBinary, _, #context{pmap = LenPMap}} = encode([{LenField#field.name, absent}],
                                                    LenField, Context#context{pmap = <<>>}),
  {LenBinary, MsgFieldsRest, Context#context{pmap = <<PMap/bits, LenPMap/bits>>}};

encode([{SeqName, MsgSeqFields} | MsgFieldsRest],
   #field_group{type = sequence, name = SeqName, instructions = [LenField | Instrs]},
   Context = #context{template = T}) ->
   {LenBin, _, Context1} =
      encode([{LenField#field.name, length(MsgSeqFields)}], LenField, Context),
   {SeqBin, #context{dicts = Dicts1}} =
      encode_seq_aux(MsgSeqFields, Context1#context{pmap = <<>>, template = T#template{instructions = Instrs}}),
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

%% ============================================================T========================================================
%% unit testing
%% ====================================================================================================================

-ifdef(TEST).

-define(template_name, <<"Fake_Template">>).

create_context(PMap) ->
   Dicts = erlang_fast_dicts:init(),
   Dicts1 = erlang_fast_dicts:new_dict(global, Dicts),
   Dicts2 = erlang_fast_dicts:new_dict(?template_name, Dicts1),
   #context{pmap = PMap, template = #template{name = ?template_name}, dicts = Dicts2}.

appendix_3_2_1_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, operator = #constant{value = 0}},
   Context = create_context(<<>>),
   Dicts = Context#context.dicts,
   ?assertMatch({<<>>, [], #context{pmap = <<>>, dicts = Dicts}}, encode([{<<"Flag">>, 0}], Field, Context)),
   ?assertThrow({error, {'ERR D3', <<"Flag">>, "Constant initial value is not the same as field one."}},
      encode([{<<"Flag">>, 99}], Field, Context)).

appendix_3_2_1_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, operator = #constant{value = 0}},
   Context = create_context(<<>>),
   Dicts = Context#context.dicts,
   ?assertMatch({<<>>, [], #context{pmap = <<1:1>>, dicts = Dicts}}, encode([{<<"Flag">>, 0}], Field, Context)),
   ?assertMatch({<<>>, [], #context{pmap = <<0:1>>, dicts = Dicts}}, encode([{<<"Flag">>, absent}], Field, Context)),
   ?assertMatch({<<>>, [{<<"Flag1">>, 0}], #context{pmap = <<0:1>>, dicts = Dicts}}, encode([{<<"Flag1">>, 0}], Field, Context)).

appendix_3_2_2_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, operator = #default{value = 0}},
   Context = create_context(<<>>),
   Dicts = Context#context.dicts,
   ?assertMatch({<<>>, [], #context{pmap = <<0:1>>, dicts = Dicts}}, encode([{<<"Flag">>, 0}], Field, Context)),
   ?assertMatch({<<2#10000001:8>>, [], #context{pmap = <<1:1>>, dicts = Dicts}}, encode([{<<"Flag">>, 1}], Field, Context)).

appendix_3_2_2_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, operator = #default{value = undef}},
   Context = create_context(<<>>),
   Dicts = Context#context.dicts,
   ?assertMatch({<<>>, [{<<"Flag1">>, 0}], #context{pmap = <<0:1>>, dicts = Dicts}}, encode([{<<"Flag1">>, 0}], Field, Context)),
   ?assertMatch({<<>>, [], #context{pmap = <<0:1>>, dicts = Dicts}}, encode([{<<"Flag">>, absent}], Field, Context)).

appendix_3_2_3_1_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Flag">>, operator = #copy{dictionary = ?template_name, value = undef, key = "key"}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Flag">>, <<"CME">>}], Field, Context),
   ?assertMatch({<<16#43, 16#4d, 16#c5>>, [], #context{pmap = <<1:1>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Flag">>, <<"CME">>}], Field, Context1),
   ?assertMatch({<<>>, [], #context{pmap = <<2#10:2>>}}, Res1),
   ?assertMatch({<<16#49, 16#53, 16#c5>>, [], #context{pmap = <<2#101:3>>}}, encode([{<<"Flag">>, <<"ISE">>}], Field, Context2)).

appendix_3_2_3_2_test() ->
   Field = #field{type = string, presence = optional, name = <<"Flag">>, operator = #copy{dictionary = ?template_name, value = undef, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = encode([{<<"Flag">>, absent}], Field, Context),
   ?assertMatch({<<2#10000000:8>>, [], #context{pmap = <<2#1:1>>}}, Res1),
   Res2 = {_, _, Context2} = encode([{<<"Flag">>, absent}], Field, Context1),
   ?assertMatch({<<>>, [], #context{pmap = <<2#10:2>>}}, Res2),
   Res3 = {_, _, _Context3} = encode([{<<"Flag">>, <<"CME">>}], Field, Context2),
   ?assertMatch({<<16#43, 16#4d, 16#c5>>, [], #context{pmap = <<2#101:3>>}}, Res3).

appendix_3_2_4_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, operator = #increment{dictionary =
         ?template_name, value = 1, key = "key"}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Flag">>, 1}], Field, Context),
   ?assertMatch({<<>>, [], #context{pmap = <<2#0:1>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Flag">>, 2}], Field, Context1),
   ?assertMatch({<<>>, [], #context{pmap = <<2#00:2>>}}, Res1),
   Res2 = {_, _, Context3} = encode([{<<"Flag">>, 4}], Field, Context2),
   ?assertMatch({<<16#84>>, [], #context{pmap = <<2#001:3>>}}, Res2),
   Res3 = {_, _, _Context4} = encode([{<<"Flag">>, 5}], Field, Context3),
   ?assertMatch({<<>>, [], #context{pmap = <<2#0010:4>>}}, Res3).

appendix_3_2_5_1_test() ->
   Field = #field{type = int32, presence = mandatory, name = <<"Price">>, operator = #delta{dictionary = ?template_name,
         key = "key"}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Price">>, 942755}], Field, Context),
   ?assertMatch({<<16#39, 16#45, 16#a3>>, [], #context{pmap = <<>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Price">>, 942750}], Field, Context1),
   ?assertMatch({<<16#fb>>, [], #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context3} = encode([{<<"Price">>, 942745}], Field, Context2),
   ?assertMatch({<<16#fb>>, [], #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, _Context4} = encode([{<<"Price">>, 942745}], Field, Context3),
   ?assertMatch({<<16#80>>, [], #context{pmap = <<>>}}, Res3).

appendix_3_2_5_2_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, operator = #delta{dictionary = ?template_name,
         key = "key"}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Price">>, {942755, -2}}], Field, Context),
   ?assertMatch({<<16#fe, 16#39, 16#45, 16#a3>>, [], #context{pmap = <<>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Price">>, {942751, -2}}], Field, Context1),
   ?assertMatch({<<16#80, 16#fc>>, [], #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, _Context3} = encode([{<<"Price">>, {942746, -2}}], Field, Context2),
   ?assertMatch({<<16#80, 16#fb>>, [], #context{pmap = <<>>}}, Res2).

appendix_3_2_5_3_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, operator = #delta{dictionary = ?template_name,
         key = "key", value = {12, 3}}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Price">>, {1210, 1}}], Field, Context),
   ?assertMatch({<<16#fe, 16#09, 16#ae>>, [], #context{pmap = <<>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Price">>, {1215, 1}}], Field, Context1),
   ?assertMatch({<<16#80, 16#85>>, [], #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, _Context3} = encode([{<<"Price">>, {1220, 1}}], Field, Context2),
   ?assertMatch({<<16#80, 16#85>>, [], #context{pmap = <<>>}}, Res2).

appendix_3_2_5_4_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Security">>, operator = #delta{dictionary = ?template_name,
         key = "key"}},
   Context = create_context(<<>>),
   Res = {_, _, Context1} = encode([{<<"Security">>, <<"GEH6">>}], Field, Context),
   ?assertMatch({<<16#80, 16#47, 16#45, 16#48, 16#b6>>, [], #context{pmap = <<>>}}, Res),
   Res1 = {_, _, Context2} = encode([{<<"Security">>, <<"GEM6">>}], Field, Context1),
   ?assertMatch({<<16#82, 16#4d, 16#b6>>, [], #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context3} = encode([{<<"Security">>, <<"ESM6">>}], Field, Context2),
   ?assertMatch({<<16#fd, 16#45, 16#d3>>, [], #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, _Context4} = encode([{<<"Security">>, <<"RSESM6">>}], Field, Context3),
   ?assertMatch({<<16#ff, 16#52, 16#d3>>, [], #context{pmap = <<>>}}, Res3).

appendix_3_2_6_3_test() ->
   Field = #field{type = decimal, presence = optional, name = <<"Value">>, operator = #decFieldOp{
         exponent = #copy{dictionary = ?template_name, key = "key_exponent"},
         mantissa = #copy{dictionary = ?template_name, key = "key_mantissa"}
      }},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = encode([{<<"Value">>, {942755, -2}}], Field, Context),
   ?assertMatch({<<16#fe, 16#39, 16#45, 16#a3>>, [], #context{pmap = <<2#11:2>>}}, Res1),
   Res2 = {_, _, _Context2} = encode([{<<"Value">>, {942760, -2}}], Field, Context1),
   ?assertMatch({<<16#39, 16#45, 16#a8>>, [], #context{pmap = <<2#1101:4>>}}, Res2).

-endif.
