-module(erlang_fast_field_decode).

-export([decode/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

%%% =========================================================================================================
%%% field decoding
%%% =========================================================================================================

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

decode(Data, #field{disp_name = DispName, presence = mandatory, operator = #constant{value = InitialValue}}, Context) ->
   {{DispName, InitialValue}, Data, Context};

decode(Data, #field{disp_name = DispName, presence = optional, operator = #constant{value = InitialValue}},
   Context = #context{pmap = <<1:1, PMapRest/bits>>}) ->
   {{DispName, InitialValue}, Data, Context#context{pmap = PMapRest}};

decode(Data, #field{disp_name = DispName, presence = optional, operator = #constant{}},
   Context = #context{pmap = <<0:1, PMapRest/bits>>}) ->
   {{DispName, absent}, Data, Context#context{pmap = PMapRest}};

%% =========================================================================================================
%% default
%% =========================================================================================================

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence, operator = #default{value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bits>>}) ->
   case InitialValue of
      undef when Presence == mandatory ->
         throw({error, {'ERR D5', FieldName, "Initial value is absent for mandatory field"}});
      undef when Presence == optional ->
         {{DispName, absent}, Data, Context#context{pmap = PMapRest}};
      InitialValue ->
         {{DispName, InitialValue}, Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #default{value = _InitialValue}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{DispName, absent}, Data1, Context#context{pmap = PMapRest}};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{DispName, Value}, Data1, Context#context{pmap = PMapRest}}
   end;

%% =========================================================================================================
%% copy
%% =========================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #copy{dictionary = D, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{DispName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {{DispName, Value}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
   end;

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence, operator = #copy{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bits>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when Presence == mandatory ->
         throw({error, {'ERR D6', FieldName, "Previous value is empty for mandatory field"}});
      empty when Presence == optional ->
         {{DispName, absent}, Data, Context#context{pmap = PMapRest}};
      undef when (Presence == mandatory) and (InitialValue == undef) -> % ERR D5
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{DispName, absent}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{DispName, InitialValue}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {{DispName, Value}, Data, Context#context{pmap = PMapRest}}
   end;

%% =========================================================================================================
%% increment
%% =========================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #increment{dictionary = Dict, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {{DispName, Value}, Data1, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, name = FieldName, disp_name = DispName, presence = Presence, operator = #increment{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = <<0:1, PMapRest/bits>>, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, {'ERR D6', FieldName, "Previous value is empty for mandatory field"}});
      empty when (Presence == optional)->
         {{DispName, absent}, Data, Context#context{pmap = PMapRest}};
      undef when (InitialValue == undef) and (Presence == mandatory) -> % ERR D5
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (InitialValue == undef) and (Presence == optional) -> % absent
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{DispName, absent}, Data, Context#context{dicts = Dicts1, pmap = PMapRest}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{DispName, InitialValue}, Data, Context#context{dicts = Dicts1, pmap = PMapRest}};
      Value ->
         NewValue = increment_value(Type, Value, 1),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewValue, Dicts),
         {{DispName, NewValue}, Data, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

%% =========================================================================================================
%% delta
%% =========================================================================================================

decode(Data, #field{type = Type, name = FieldName, disp_name = DispName, presence = Presence, operator = #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}) ->
   case decode_delta(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{DispName, absent}, Data1, Context};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, {'ERR D6', FieldName, "Previous value is empty for delta operator"}});
            undef ->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{DispName, NewVal}, Data1, Context#context{dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{DispName, NewVal}, Data1, Context#context{dicts = Dicts1}}
         end
   end;

%% =========================================================================================================
%% tail
%% =========================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, pmap = <<1:1, PMapRest/bits>>,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{DispName, absent}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            PrevValue when (PrevValue == empty) or (PrevValue == undef)->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{DispName, NewVal}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {{DispName, NewVal}, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
         end
   end;

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence, operator = #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bits>>,
      application = App, template = #template{name = TemplateName}}) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, {'ERR D7', FieldName, "Previous value is empty for mandatory field"}});
      empty when (Presence == optional)->
         {{DispName, absent}, Data, Context#context{pmap = PMapRest}};
      undef when (InitialValue == undef) andalso (Presence == mandatory) ->
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (InitialValue == undef) andalso (Presence == optional) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {{DispName, absent}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {{DispName, InitialValue}, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {{DispName, Value}, Data, Context#context{pmap = PMapRest}}
   end;

%% =========================================================================================================
%% decFieldOp
%% =========================================================================================================

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence, operator = #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context) ->
   case decode(Data, #field{type = int32, name = FieldName, disp_name = DispName, presence = Presence, operator = ExpOp}, Context) of
      R = {{DispName, absent}, _, _} ->
         R;
      {{DispName, Exponent}, Data1, Context1} ->
         {{DispName, Mantissa}, Data2, Context2} =
            decode(Data1, #field{type = int64, name = FieldName, disp_name = DispName, presence = mandatory, operator = MantOp}, Context1),
         {{DispName, {Mantissa, Exponent}}, Data2, Context2}
   end;

%% =========================================================================================================
%% no operator
%% =========================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = undef}, Context = #context{logger = L}) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {{DispName, absent}, Data1, Context};
      {Value, Err, Data1} ->
         L(Err, Value),
         {{DispName, Value}, Data1, Context}
   end;

%% =========================================================================================================
%% group decoding
%% =========================================================================================================

decode(Data, #field_group{type = group, presence = optional, disp_name = DispName}, Context = #context{pmap = <<0:1, PMapRest/bits>>}) ->
   {{DispName, absent}, Data, Context#context{pmap = PMapRest}};

decode(Data,
   #field_group{type = group, disp_name = DispName, presence = Presence, need_pmap = NeedPMap, instructions = Instrs},
      Context = #context{pmap = PMap = <<PresenceBit:1, PMapRest/bits>>})
      when (Presence == mandatory) or ((Presence == optional) andalso (PresenceBit == 1)) ->
   {Data1, Context1} =
   case NeedPMap of
      true ->
         erlang_fast_segment:decode_pmap(Data, Context);
      false ->
         {Data, Context}
   end,
   {Msg, Data2, #context{dicts = Dicts}} =
      erlang_fast_segment:decode_fields(Data1, Context1#context.template#template{instructions = Instrs}),
      {{DispName, Msg}, Data2, Context#context{pmap = if (Presence == optional) -> PMapRest; true -> PMap end, dicts = Dicts}};

%% =========================================================================================================
%% sequence decoding
%% =========================================================================================================
decode(Data, #field_group{type = sequence, disp_name = DispName, instructions = []}, Context) ->
   {{DispName, absent}, Data, Context};

decode(Data, #field_group{type = sequence, disp_name = DispName, presence = Presence, need_pmap = NeedPMap, instructions = Instructions}, Context) ->
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
         {{DispName, absent}, Data1, Context1};
      LenValue ->
         Instrs = case (hd(Instructions))#field.type == length of
            true ->
               tl(Instructions);
            false ->
               Instructions
         end,
         {Sequence, Data2, #context{dicts = Dicts}} = decode_sequence_aux(LenValue, Data1, NeedPMap,
            Context1#context{template = Context1#context.template#template{instructions = Instrs}}),
         {{DispName, Sequence}, Data2, Context1#context{dicts = Dicts}}
   end;

%% =========================================================================================================
%% decoding terminator
%% =========================================================================================================
decode(_, Instr, _) ->
   throw({error, {unknown_field, Instr}}).


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

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(TEST).

-define(template_name, <<"Fake_Template">>).

create_context(PMap) ->
   Dicts = erlang_fast_dicts:init(),
   Dicts1 = erlang_fast_dicts:new_dict(global, Dicts),
   Dicts2 = erlang_fast_dicts:new_dict(?template_name, Dicts1),
   #context{pmap = PMap, template = #template{name = ?template_name}, dicts = Dicts2, logger = fun(_A, _B) -> ok end}.

appendix_3_2_1_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>, operator = #constant{value = 0}},
   Context = create_context(<<>>),
   Res = decode(<<>>, Field, Context),
   ?assertMatch({{<<"Flag">>, 0}, <<>>, #context{pmap = <<>>}}, Res).

appendix_3_2_1_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>, operator = #constant{value = 0}},
   Context = create_context(<<2#10:2>>),
   Res1 = {_, _, Context1} = decode(<<>>, Field, Context),
   ?assertMatch({{<<"Flag">>, 0}, <<>>, #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, _Context2} = decode(<<>>, Field, Context1),
   ?assertMatch({{<<"Flag">>, absent}, <<>>, #context{pmap = <<>>}}, Res2).

appendix_3_2_2_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>, operator = #default{value = 0}},
   Context = create_context(<<2#01:2>>),
   Res1 = {_, _, Context1} = decode(<<16#81>>, Field, Context),
   ?assertMatch({{<<"Flag">>, 0}, <<16#81>>, #context{pmap = <<2#1:1>>}}, Res1),
   Res2 = {_, _, _Context2} = decode(<<16#81>>, Field, Context1),
   ?assertMatch({{<<"Flag">>, 1}, <<>>, #context{pmap = <<>>}}, Res2).

appendix_3_2_2_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #default{value = undef}},
   Context = create_context(<<2#0:1>>),
   Res1 = decode(<<>>, Field, Context),
   ?assertMatch({{<<"Flag">>, absent}, <<>>, #context{pmap = <<>>}}, Res1).

appendix_3_2_3_1_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #copy{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<2#101:3>>),
   Res1 = {_, _, Context1} = decode(<<16#43, 16#4d, 16#c5, 16#49, 16#53, 16#c5>>, Field, Context),
   ?assertMatch({{<<"Flag">>, <<"CME">>}, <<16#49, 16#53, 16#c5>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#49, 16#53, 16#c5>>, Field, Context1),
   ?assertMatch({{<<"Flag">>, <<"CME">>}, <<16#49, 16#53, 16#c5>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#49, 16#53, 16#c5>>, Field, Context2),
   ?assertMatch({{<<"Flag">>, <<"ISE">>}, <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_3_2_test() ->
   Field = #field{type = string, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #copy{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<2#101:3>>),
   Res1 = {_, _, Context1} = decode(<<16#80, 16#43, 16#4d, 16#c5>>, Field, Context),
   ?assertMatch({{<<"Flag">>, absent}, <<16#43, 16#4d, 16#c5>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#43, 16#4d, 16#c5>>, Field, Context1),
   ?assertMatch({{<<"Flag">>, absent}, <<16#43, 16#4d, 16#c5>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#43, 16#4d, 16#c5>>, Field, Context2),
   ?assertMatch({{<<"Flag">>, <<"CME">>}, <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_4_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>,
      operator = #increment{dictionary = ?template_name, value = 1, key = "key"}},
   Context = create_context(<<2#001:3>>),
   Res1 = {_, _, Context1} = decode(<<16#84>>, Field, Context),
   ?assertMatch({{<<"Flag">>, 1}, <<16#84>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#84>>, Field, Context1),
   ?assertMatch({{<<"Flag">>, 2}, <<16#84>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#84>>, Field, Context2),
   ?assertMatch({{<<"Flag">>, 4}, <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_5_1_test() ->
   Field = #field{type = int32, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#39, 16#45, 16#a3, 16#fb, 16#fb, 16#80>>, Field, Context),
   ?assertMatch({{<<"Price">>, 942755}, <<16#fb, 16#fb, 16#80>>, #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#fb, 16#fb, 16#80>>, Field, Context1),
   ?assertMatch({{<<"Price">>, 942750}, <<16#fb, 16#80>>, #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, Context3} = decode(<<16#fb, 16#80>>, Field, Context2),
   ?assertMatch({{<<"Price">>, 942745}, <<16#80>>, #context{pmap = <<>>}}, Res3),
   Res4 = {_, _, _Context4} = decode(<<16#80>>, Field, Context3),
   ?assertMatch({{<<"Price">>, 942745}, <<>>, #context{pmap = <<>>}}, Res4).

appendix_3_2_5_2_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#fe, 16#39, 16#45, 16#a3, 16#80, 16#fc, 16#80, 16#fb>>, Field, Context),
   ?assertMatch({{<<"Price">>, {942755, -2}}, <<16#80, 16#fc, 16#80, 16#fb>>, #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#80, 16#fc, 16#80, 16#fb>>, Field, Context1),
   ?assertMatch({{<<"Price">>, {942751, -2}}, <<16#80, 16#fb>>, #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#80, 16#fb>>, Field, Context2),
   ?assertMatch({{<<"Price">>, {942746, -2}}, <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_5_3_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, value = {12, 3}, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#fe, 16#09, 16#ae, 16#80, 16#85, 16#80, 16#85>>, Field, Context),
   ?assertMatch({{<<"Price">>, {1210, 1}}, <<16#80, 16#85, 16#80, 16#85>>, #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#80, 16#85, 16#80, 16#85>>, Field, Context1),
   ?assertMatch({{<<"Price">>, {1215, 1}}, <<16#80, 16#85>>, #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#80, 16#85>>, Field, Context2),
   ?assertMatch({{<<"Price">>, {1220, 1}}, <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_5_4_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Security">>, disp_name = <<"Security">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#80, 16#47, 16#45, 16#48, 16#b6, 16#82, 16#4d, 16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field, Context),
   ?assertMatch({{<<"Security">>, <<"GEH6">>}, <<16#82, 16#4d, 16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, #context{pmap = <<>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#82, 16#4d, 16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field, Context1),
   ?assertMatch({{<<"Security">>, <<"GEM6">>}, <<16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, #context{pmap = <<>>}}, Res2),
   Res3 = {_, _, Context3} = decode(<<16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field, Context2),
   ?assertMatch({{<<"Security">>, <<"ESM6">>}, <<16#ff, 16#52, 16#d3>>, #context{pmap = <<>>}}, Res3),
   Res4 = {_, _, _Context4} = decode(<<16#ff, 16#52, 16#d3>>, Field, Context3),
   ?assertMatch({{<<"Security">>, <<"RSESM6">>}, <<>>, #context{pmap = <<>>}}, Res4).

-endif.
