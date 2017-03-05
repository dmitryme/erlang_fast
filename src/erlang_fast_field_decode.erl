-module(erlang_fast_field_decode).

-export([decode/4]).

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

%%% ====================================================================================================================
%%% field decoding
%%% ====================================================================================================================

% PMap has an infinite tail of zero bits, even if PMap has no bits
decode(Data, Instr, Context = #context{pmap = <<>>}, Msg) ->
   decode(Data, Instr, Context#context{pmap = <<0:1>>}, Msg);

%% =====================================================================================================================
%% templateRef
%% =====================================================================================================================

decode(Data, #templateRef{name = undef}, Context, Msg) ->
   {Msg1, Data1, #context{dicts = D}} = erlang_fast_segment:decode(Data, Context, Msg),
   {Msg1, Data1, Context#context{dicts = D}};

decode(Data, #templateRef{name = Name}, Context = #context{template = T = #template{instructions = Instrs}}, Msg) ->
   TemplateRef = erlang_fast_templates:get_by_name(Name, Context#context.templates#templates.tlist),
   {Msg, Data, Context#context{template = T#template{instructions = TemplateRef#template.instructions ++ Instrs}}};

%% =====================================================================================================================
%% templateRef
%% =====================================================================================================================

decode(Data, #typeRef{name = TypeName}, Context, Msg) ->
   {Msg, Data, Context#context{application = TypeName}};

%% =====================================================================================================================
%% constant
%% =====================================================================================================================

decode(Data, #field{disp_name = DispName, presence = mandatory, operator = #constant{value = InitialValue}},
       Context, Msg) ->
   {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context};

decode(Data, #field{disp_name = DispName, presence = optional, operator = #constant{value = InitialValue}},
   Context = #context{pmap = <<1:1, PMapRest/bits>>}, Msg) ->
   {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context#context{pmap = PMapRest}};

decode(Data, #field{presence = optional, operator = #constant{}},
   Context = #context{pmap = <<0:1, PMapRest/bits>>}, Msg) ->
   {Msg, Data, Context#context{pmap = PMapRest}};

%% =====================================================================================================================
%% default
%% =====================================================================================================================

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence, operator = #default{value = InitialValue}},
   Context = #context{pmap = <<0:1, PMapRest/bits>>}, Msg) ->
   case InitialValue of
      undef when Presence == mandatory ->
         throw({error, {'ERR D5', FieldName, "Initial value is absent for mandatory field"}});
      undef when Presence == optional ->
         {Msg, Data, Context#context{pmap = PMapRest}};
      InitialValue ->
         {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context#context{pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #default{value = _InitialValue}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>}, Msg) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {Msg, Data1, Context#context{pmap = PMapRest}};
      {Value, Err, Data1} ->
         L(Err, Value),
         {erlang_fast_msg:add(Msg, DispName, Value), Data1, Context#context{pmap = PMapRest}}
   end;

%% =====================================================================================================================
%% copy
%% =====================================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = #copy{dictionary = D, key = Key}},
   Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}, Msg) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {Msg, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, Value, Dicts),
         {erlang_fast_msg:add(Msg, DispName, Value), Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
   end;

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence,
                    operator = #copy{dictionary = D, key = Key, value = InitialValue}},
       Context = #context{pmap = <<0:1, PMapRest/bits>>, dicts = Dicts,
      application = App, template = #template{name = TemplateName}}, Msg) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when Presence == mandatory ->
         throw({error, {'ERR D6', FieldName, "Previous value is empty for mandatory field"}});
      empty when Presence == optional ->
         {Msg, Data, Context#context{pmap = PMapRest}};
      undef when (Presence == mandatory) and (InitialValue == undef) -> % ERR D5
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (Presence == optional) and (InitialValue == undef) -> % it becomes empty
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {Msg, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {erlang_fast_msg:add(Msg, DispName, Value), Data, Context#context{pmap = PMapRest}}
   end;

%% =====================================================================================================================
%% increment
%% =====================================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence,
                    operator = #increment{dictionary = Dict, key = Key}},
       Context = #context{logger = L, pmap = <<1:1, PMapRest/bits>>, dicts = Dicts, application = App,
      template = #template{name = TemplateName}}, Msg) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {Value, Err, Data1} ->
         L(Err, Value),
         Dicts1 = erlang_fast_dicts:put_value(select_dict(Dict, TemplateName, App), Key, Value, Dicts),
         {erlang_fast_msg:add(Msg, DispName, Value), Data1, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

decode(Data, #field{type = Type, name = FieldName, disp_name = DispName, presence = Presence,
                    operator = #increment{dictionary = D, key = Key, value = InitialValue}},
      Context = #context{pmap = <<0:1, PMapRest/bits>>, dicts = Dicts, application = App,
                         template = #template{name = TemplateName}}, Msg) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, {'ERR D6', FieldName, "Previous value is empty for mandatory field"}});
      empty when (Presence == optional)->
         {Msg, Data, Context#context{pmap = PMapRest}};
      undef when (InitialValue == undef) and (Presence == mandatory) -> % ERR D5
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (InitialValue == undef) and (Presence == optional) -> % absent
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {Msg, Data, Context#context{dicts = Dicts1, pmap = PMapRest}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context#context{dicts = Dicts1, pmap = PMapRest}};
      Value ->
         NewValue = increment_value(Type, Value, 1),
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewValue, Dicts),
         {erlang_fast_msg:add(Msg, DispName, NewValue), Data, Context#context{dicts = Dicts1, pmap = PMapRest}}
   end;

%% =====================================================================================================================
%% delta
%% =====================================================================================================================

decode(Data, #field{type = Type, name = FieldName, disp_name = DispName, presence = Presence,
                    operator = #delta{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, application = App, template = #template{name = TemplateName}}, Msg) ->
   case decode_delta(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {Msg, Data1, Context};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         Dict = select_dict(D, TemplateName, App),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            empty ->
               throw({error, {'ERR D6', FieldName, "Previous value is empty for delta operator"}});
            undef ->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {erlang_fast_msg:add(Msg, DispName, NewVal), Data1, Context#context{dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {erlang_fast_msg:add(Msg, DispName, NewVal), Data1, Context#context{dicts = Dicts1}}
         end
   end;

%% =====================================================================================================================
%% tail
%% =====================================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence,
                    operator = #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{logger = L, dicts = Dicts, pmap = <<1:1, PMapRest/bits>>,
      application = App, template = #template{name = TemplateName}}, Msg) ->
   Dict = select_dict(D, TemplateName, App),
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {Msg, Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
      {Delta, Err, Data1} ->
         L(Err, Delta),
         case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
            PrevValue when (PrevValue == empty) or (PrevValue == undef)->
               NewVal = apply_delta(InitialValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {erlang_fast_msg:add(Msg, DispName, NewVal), Data1, Context#context{pmap = PMapRest, dicts = Dicts1}};
            PrevValue ->
               NewVal = apply_delta(PrevValue, Delta),
               Dicts1 = erlang_fast_dicts:put_value(Dict, Key, NewVal, Dicts),
               {erlang_fast_msg:add(Msg, DispName, NewVal), Data1, Context#context{pmap = PMapRest, dicts = Dicts1}}
         end
   end;

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence,
                    operator = #tail{dictionary = D, key = Key, value = InitialValue}},
   Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bits>>,
      application = App, template = #template{name = TemplateName}}, Msg) ->
   Dict = select_dict(D, TemplateName, App),
   case erlang_fast_dicts:get_value(Dict, Key, Dicts) of
      empty when (Presence == mandatory)->
         throw({error, {'ERR D7', FieldName, "Previous value is empty for mandatory field"}});
      empty when (Presence == optional)->
         {Msg, Data, Context#context{pmap = PMapRest}};
      undef when (InitialValue == undef) andalso (Presence == mandatory) ->
         throw({error, {'ERR D5', FieldName, "no initial value"}});
      undef when (InitialValue == undef) andalso (Presence == optional) ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, empty, Dicts),
         {Msg, Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      undef ->
         Dicts1 = erlang_fast_dicts:put_value(Dict, Key, InitialValue, Dicts),
         {erlang_fast_msg:add(Msg, DispName, InitialValue), Data, Context#context{pmap = PMapRest, dicts = Dicts1}};
      Value ->
         {erlang_fast_msg:add(Msg, DispName, Value), Data, Context#context{pmap = PMapRest}}
   end;

%% =====================================================================================================================
%% decFieldOp
%% =====================================================================================================================

decode(Data, #field{name = FieldName, disp_name = DispName, presence = Presence,
                    operator = #decFieldOp{exponent = ExpOp, mantissa = MantOp}}, Context, Msg) ->
   {Msg1, Data1, Context1} = decode(Data, #field{type = int32, name = FieldName, disp_name = DispName,
                            presence = Presence, operator = ExpOp}, Context, erlang_fast_msg:new(Context)),
   case erlang_fast_msg:get(Msg1, DispName, not_found) of
      not_found ->
         {Msg, Data1, Context1};
      Exponent ->
         {Msg2, Data2, Context2} = decode(Data1, #field{type = int64, name = FieldName, disp_name = DispName,
                                 presence = mandatory, operator = MantOp}, Context1, erlang_fast_msg:new(Context)),
         Mantissa = erlang_fast_msg:get(Msg2, DispName, 0),
         {erlang_fast_msg:add(Msg, DispName, {Mantissa, Exponent}), Data2, Context2}
   end;

%% =====================================================================================================================
%% no operator
%% =====================================================================================================================

decode(Data, #field{type = Type, disp_name = DispName, presence = Presence, operator = undef},
       Context = #context{logger = L}, Msg) ->
   case decode_type(Type, Data, is_nullable(Presence)) of
      {null, _, Data1} ->
         {Msg, Data1, Context};
      {Value, Err, Data1} ->
         L(Err, Value),
         {erlang_fast_msg:add(Msg, DispName, Value), Data1, Context}
   end;

%% =====================================================================================================================
%% group decoding
%% =====================================================================================================================

decode(Data, #field_group{type = group, presence = optional},
       Context = #context{pmap = <<0:1, PMapRest/bits>>}, Msg) ->
   {Msg, Data, Context#context{pmap = PMapRest}};

decode(Data,
   #field_group{type = group, presence = Presence, need_pmap = NeedPMap, instructions = Instrs},
      Context = #context{pmap = PMap = <<PresenceBit:1, PMapRest/bits>>}, Msg)
      when (Presence == mandatory) or ((Presence == optional) andalso (PresenceBit == 1)) ->
   {Data1, Context1} =
   case NeedPMap of
      true ->
         erlang_fast_segment:decode_pmap(Data, Context);
      false ->
         {Data, Context}
   end,
   {Msg1, Data2, #context{dicts = Dicts}} =
      erlang_fast_segment:decode_fields(Data1, Context1#context.template#template{instructions = Instrs},
                                        erlang_fast_msg:new(Context1)),
      {erlang_fast_msg:merge(Msg, Msg1), Data2, Context#context{pmap = if (Presence == optional) -> PMapRest;
                                                               true -> PMap end, dicts = Dicts}};

%% =====================================================================================================================
%% sequence decoding
%% =====================================================================================================================
decode(Data, #field_group{type = sequence, disp_name = _DispName, instructions = []}, Context, Msg) ->
   {Msg, Data, Context};

decode(Data, #field_group{type = sequence, disp_name = DispName, need_pmap = NeedPMap,
                          instructions = [LengthField = #field{disp_name = LenDispName} | Instructions]},
       Context, Msg) ->
   {Msg1, Data1, Context1} = decode(Data, LengthField, Context, erlang_fast_msg:new(Context)),
   case erlang_fast_msg:get(Msg1, LenDispName, absent) of
      absent ->
         {Msg, Data1, Context1};
      LenValue ->
         {Sequence, Data2, #context{dicts = Dicts}} = decode_sequence_aux(LenValue, Data1, NeedPMap,
            Context1#context{template = Context1#context.template#template{instructions = Instructions}}),
         {erlang_fast_msg:add(Msg, DispName, Sequence), Data2, Context1#context{dicts = Dicts}}
   end;

%% =====================================================================================================================
%% decoding terminator
%% =====================================================================================================================
decode(_, Instr, _, _) ->
   throw({error, {unknown_field, Instr}}).


%% =====================================================================================================================
%% decoding terminator
%% =====================================================================================================================
decode_sequence_aux(Length, Data, NeedPMap, Context) ->
   decode_sequence_aux(Length, 0, Data, NeedPMap, Context, erlang_fast_msg:new(Context)).

decode_sequence_aux(Length, Count, Data, _NeedPMap, Context, Msg) when Count == Length ->
   {erlang_fast_msg:reverse(Msg), Data, Context};

decode_sequence_aux(Length, Count, Data, NeedPMap, Context = #context{template = Template}, Msg) ->
   case NeedPMap of
      true ->
         {Data1, Context1} = erlang_fast_segment:decode_pmap(Data, Context),
         {Msg1, Data2, Context2} = erlang_fast_segment:decode_fields(Data1, Context1, erlang_fast_msg:new(Context)),
         {Msg2, Data3, Context3} = decode_sequence_aux(Length, Count + 1, Data2, NeedPMap,
                                                       Context2#context{template = Template},
                                                       erlang_fast_msg:add_grp(Msg, Count, Msg1)),
         {Msg2, Data3, Context3};
      false ->
         {Msg1, Data1, Context1} = erlang_fast_segment:decode_fields(Data, Context, erlang_fast_msg:new(Context)),
         {Msg2, Data2, Context2} = decode_sequence_aux(Length, Count + 1, Data1, NeedPMap,
                                                       Context1#context{template = Template},
                                                       erlang_fast_msg:add_grp(Msg, Count, Msg1)),
         {Msg2, Data2, Context2}
   end.

%% =====================================================================================================================
%% unit testing
%% =====================================================================================================================

-ifdef(TEST).

-define(template_name, <<"Fake_Template">>).

create_context(PMap) ->
   Dicts = erlang_fast_dicts:init(),
   Dicts1 = erlang_fast_dicts:new_dict(global, Dicts),
   Dicts2 = erlang_fast_dicts:new_dict(?template_name, Dicts1),
   #context{pmap = PMap, options = [], template = #template{name = ?template_name},
            dicts = Dicts2, logger = fun(_A, _B) -> ok end}.

appendix_3_2_1_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>,
                  operator = #constant{value = 0}},
   Context = create_context(<<>>),
   Res = decode(<<>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 0}], <<>>, #context{pmap = <<2#0:1>>}}, Res).

appendix_3_2_1_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>,
                  operator = #constant{value = 0}},
   Context = create_context(<<2#10:2>>),
   Res1 = {_, _, Context1} = decode(<<>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 0}], <<>>, #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, _Context2} = decode(<<>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[], <<>>, #context{pmap = <<>>}}, Res2).

appendix_3_2_2_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>,
                  operator = #default{value = 0}},
   Context = create_context(<<2#01:2>>),
   Res1 = {_, _, Context1} = decode(<<16#81>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 0}], <<16#81>>, #context{pmap = <<2#1:1>>}}, Res1),
   Res2 = {_, _, _Context2} = decode(<<16#81>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 1}], <<>>, #context{pmap = <<>>}}, Res2).

appendix_3_2_2_2_test() ->
   Field = #field{type = uInt32, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #default{value = undef}},
   Context = create_context(<<2#0:1>>),
   Res1 = decode(<<>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[], <<>>, #context{pmap = <<>>}}, Res1).

appendix_3_2_3_1_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #copy{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<2#101:3>>),
   Res1 = {_, _, Context1} = decode(<<16#43, 16#4d, 16#c5, 16#49, 16#53, 16#c5>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, <<"CME">>}], <<16#49, 16#53, 16#c5>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#49, 16#53, 16#c5>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, <<"CME">>}], <<16#49, 16#53, 16#c5>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#49, 16#53, 16#c5>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, <<"ISE">>}], <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_3_2_test() ->
   Field = #field{type = string, presence = optional, name = <<"Flag">>, disp_name = <<"Flag">>, operator =
      #copy{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<2#101:3>>),
   Res1 = {_, _, Context1} = decode(<<16#80, 16#43, 16#4d, 16#c5>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[], <<16#43, 16#4d, 16#c5>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#43, 16#4d, 16#c5>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[], <<16#43, 16#4d, 16#c5>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#43, 16#4d, 16#c5>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, <<"CME">>}], <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_4_1_test() ->
   Field = #field{type = uInt32, presence = mandatory, name = <<"Flag">>, disp_name = <<"Flag">>,
      operator = #increment{dictionary = ?template_name, value = 1, key = "key"}},
   Context = create_context(<<2#001:3>>),
   Res1 = {_, _, Context1} = decode(<<16#84>>, Field, Context, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 1}], <<16#84>>, #context{pmap = <<2#01:2>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#84>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 2}], <<16#84>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#84>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Flag">>, 4}], <<>>, #context{pmap = <<>>}}, Res3).

appendix_3_2_5_1_test() ->
   Field = #field{type = int32, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#39, 16#45, 16#a3, 16#fb, 16#fb, 16#80>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, 942755}], <<16#fb, 16#fb, 16#80>>, #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#fb, 16#fb, 16#80>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, 942750}], <<16#fb, 16#80>>, #context{pmap = <<2#0:1>>}}, Res2),
   Res3 = {_, _, Context3} = decode(<<16#fb, 16#80>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, 942745}], <<16#80>>, #context{pmap = <<2#0:1>>}}, Res3),
   Res4 = {_, _, _Context4} = decode(<<16#80>>, Field, Context3, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, 942745}], <<>>, #context{pmap = <<2#0:1>>}}, Res4).

appendix_3_2_5_2_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#fe, 16#39, 16#45, 16#a3, 16#80, 16#fc, 16#80, 16#fb>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {942755, -2}}], <<16#80, 16#fc, 16#80, 16#fb>>, #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#80, 16#fc, 16#80, 16#fb>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {942751, -2}}], <<16#80, 16#fb>>, #context{pmap = <<2#0:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#80, 16#fb>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {942746, -2}}], <<>>, #context{pmap = <<2#0:1>>}}, Res3).

appendix_3_2_5_3_test() ->
   Field = #field{type = decimal, presence = mandatory, name = <<"Price">>, disp_name = <<"Price">>,
      operator = #delta{dictionary = ?template_name, value = {12, 3}, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#fe, 16#09, 16#ae, 16#80, 16#85, 16#80, 16#85>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {1210, 1}}], <<16#80, 16#85, 16#80, 16#85>>, #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#80, 16#85, 16#80, 16#85>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {1215, 1}}], <<16#80, 16#85>>, #context{pmap = <<2#0:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#80, 16#85>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Price">>, {1220, 1}}], <<>>, #context{pmap = <<2#0:1>>}}, Res3).

appendix_3_2_5_4_test() ->
   Field = #field{type = string, presence = mandatory, name = <<"Security">>, disp_name = <<"Security">>,
      operator = #delta{dictionary = ?template_name, key = "key"}},
   Context = create_context(<<>>),
   Res1 = {_, _, Context1} = decode(<<16#80, 16#47, 16#45, 16#48, 16#b6, 16#82, 16#4d,
                                      16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Security">>, <<"GEH6">>}], <<16#82, 16#4d, 16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>,
                 #context{pmap = <<2#0:1>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#82, 16#4d, 16#b6, 16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field,
                                    Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Security">>, <<"GEM6">>}], <<16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>,
                 #context{pmap = <<2#0:1>>}}, Res2),
   Res3 = {_, _, Context3} = decode(<<16#fd, 16#45, 16#d3, 16#ff, 16#52, 16#d3>>, Field, Context2,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Security">>, <<"ESM6">>}], <<16#ff, 16#52, 16#d3>>, #context{pmap = <<2#0:1>>}}, Res3),
   Res4 = {_, _, _Context4} = decode(<<16#ff, 16#52, 16#d3>>, Field, Context3, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Security">>, <<"RSESM6">>}], <<>>, #context{pmap = <<2#0:1>>}}, Res4).

appendix_3_2_6_3_test() ->
   Field = #field{type = decimal, presence = optional, name = <<"Value">>, disp_name = <<"Value">>,
                  operator = #decFieldOp{
                                exponent = #copy{dictionary = ?template_name, key = "key_exponent"},
                                mantissa = #copy{dictionary = ?template_name, key = "key_mantissa"}
      }},
   Context = create_context(<<2#11011:5>>),
   Res1 = {_, _, Context1} = decode(<<16#fe, 16#39, 16#45, 16#a3, 16#39, 16#45, 16#a8, 16#80>>, Field, Context,
                                    erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Value">>, {942755, -2}}], <<16#39, 16#45, 16#a8, 16#80>>, #context{pmap = <<2#011:3>>}}, Res1),
   Res2 = {_, _, Context2} = decode(<<16#39, 16#45, 16#a8, 16#80>>, Field, Context1, erlang_fast_msg:new(Context)),
   ?assertMatch({[{<<"Value">>, {942760, -2}}], <<16#80>>, #context{pmap = <<2#1:1>>}}, Res2),
   Res3 = {_, _, _Context3} = decode(<<16#80>>, Field, Context2, erlang_fast_msg:new(Context)),
   ?assertMatch({[], <<>>, #context{pmap = <<>>}}, Res3).

-endif.
