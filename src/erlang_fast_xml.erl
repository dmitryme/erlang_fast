-module(erlang_fast_xml).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-compile([export_all]).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlang_fast_template.hrl").

-import(erlang_fast_xml_utils, [get_attribute/2, get_attribute/3]).

parse(XmlFile) ->
   {RootElem, []} = xmerl_scan:file(XmlFile),
   Dicts = erlang_fast_dict:init(),
   DictName = string_to_dic(get_attribute(dictionary, RootElem, global)),
   Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
   {Dicts2, TList} = parse_template(RootElem, Dicts1, DictName, gb_trees:empty()),
   {Dicts2, #templates{
         ns = get_attribute(ns, RootElem),
         templateNs = get_attribute(templateNs, RootElem),
         dictionary = DictName,
         tlist = TList}}.

parse_template([], Dicts, _DefDict, Templates) ->
   {Dicts, Templates};

parse_template(#xmlElement{name = templates, content = Childs}, Dicts, DefDict, Templates) ->
   parse_template(Childs, Dicts, DefDict, Templates);

parse_template([XmlElem = #xmlElement{content = Childs} | Rest], Dicts, DefDict, Templates) ->
   DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
   Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
   {Dicts2, Instructions, _} = parse_instruction(Childs, Dicts1, DictName),
   Template = #template{
                  name = get_attribute(name, XmlElem),
                  templateNs = get_attribute(templateNs, XmlElem),
                  id = string_to_id(get_attribute(id, XmlElem)),
                  ns = get_attribute(ns, XmlElem),
                  dictionary = DictName,
                  typeRef = parse_typeRef(Childs),
                  instructions = Instructions},
   Templates2 = gb_trees:insert(Template#template.id, Template, Templates),
   parse_template(Rest, Dicts2, DefDict, Templates2);

parse_template([#xmlText{} | Rest], Dicts, DefDict, Templates) ->
   parse_template(Rest, Dicts, DefDict, Templates).

parse_typeRef([]) ->
   undef;
parse_typeRef([I = #xmlElement{name = typeRef} | _Tail]) ->
   get_attribute(name, I);
parse_typeRef([_|Tail]) ->
   parse_typeRef(Tail).

parse_instruction([], Dicts, _DefDict) ->
   {Dicts, [], false};

parse_instruction([I = #xmlElement{name = templateRef} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts1, [#templateRef{
            name = get_attribute(name, I),
            templateNs = get_attribute(templateNs, I),
            ns = get_attribute(ns, I)} | Instructions], NeedPMap or false};

parse_instruction([I = #xmlElement{name = string = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   Instr =  case get_attribute(charset, I, ascii) of
               ascii ->
                  #string{
                     name = get_attribute(name, I),
                     ns = get_attribute(ns, I),
                     id = string_to_id(get_attribute(id, I)),
                     presence = string_to_presence(Presence),
                     operator = Operator};
               unicode ->
                  #unicode{
                     name = get_attribute(name, I),
                     ns = get_attribute(ns, I),
                     id = string_to_id(get_attribute(id, I)),
                     presence = string_to_presence(Presence),
                     operator = Operator}
            end,
   {Dicts2, [Instr | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = int32 = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#int32{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = 'Int64' = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#int64{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = uInt32 = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#uInt32{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = uInt64 = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#uInt64{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = length = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   {Dicts2, [#length{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            operator = Operator} | Instructions], NeedPMap or false};

parse_instruction([I = #xmlElement{name = decimal, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_dec_op(Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#decimal{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = byteVector = T, content = Childs} | Tail], Dicts, DefDict) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict),
   {Dicts2, Operator} = parse_op(T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#byteVector{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = sequence, content = Childs} | Tail], Dicts, DefDict) ->
   DictName = string_to_dic(get_attribute(dictionary, I, DefDict)),
   {Dicts1, SeqInstructions, NeedPMap} = parse_instruction(Childs, Dicts, DictName),
   {Dicts2, Instructions, _NeedPMap} = parse_instruction(Tail, Dicts1, DictName),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#sequence{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            dictionary = DictName,
            need_pmap = NeedPMap,
            typeRef = parse_typeRef(Childs),
            instructions = SeqInstructions} | Instructions], need_pmap(Presence)};

parse_instruction([I = #xmlElement{name = group, content = Childs} | Tail], Dicts, DefDict) ->
   DictName = string_to_dic(get_attribute(dictionary, I, DefDict)),
   {Dicts1, GroupInstructions, NeedPMap} = parse_instruction(Childs, Dicts, DictName),
   {Dicts2, Instructions, _NeedPMap} = parse_instruction(Tail, Dicts1, DictName),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#group{
            name = get_attribute(name, I),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            dictionary = DictName,
            need_pmap = NeedPMap,
            typeRef = parse_typeRef(Childs),
            instructions = GroupInstructions} | Instructions], need_pmap(Presence)};

parse_instruction([#xmlText{} | Tail], Dicts, DefDict) ->
   parse_instruction(Tail, Dicts, DefDict);

parse_instruction([I | _Tail], _, _) ->
   erlang:error({unknown_tag, I}).

parse_dec_op(Childs, Dicts, DefDict) ->
   Res = lists:foldr(fun(#xmlElement{name = exponent, content = C}, {D, DecFieldOp}) ->
            {Dicts1, Operator} = parse_op(int32, C, D, DefDict),
            {Dicts1, DecFieldOp#decFieldOp{exponent = Operator}};
         (#xmlElement{name = mantissa, content = C}, {D, DecFieldOp}) ->
            {Dicts1, Operator} = parse_op(int64, C, D, DefDict),
            {Dicts1, DecFieldOp#decFieldOp{mantissa = Operator}};
         (_, Acc) ->
            Acc end,
      {Dicts, #decFieldOp{exponent = undef, mantissa = undef}}, Childs),
   case Res of
      {Dicts, #decFieldOp{exponent = undef, mantissa = undef}} ->
         parse_op(decimal, Childs, Dicts, DefDict);
      _ ->
         Res
   end.

parse_op(Type, Childs, Dicts, DefDict) ->
   case lists:keyfind(xmlElement, 1, Childs) of
      false ->
         {Dicts, undef};
      XmlElem = #xmlElement{name = constant} ->
         {Dicts, #constant{value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = default} ->
         {Dicts, #default{value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = copy} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
         {Dicts1, #copy{
               dictionary = DictName,
               key = get_attribute(key, XmlElem),
               ns = get_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = increment} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
         {Dicts1, #increment{
               dictionary = DictName,
               key = get_attribute(key, XmlElem),
               ns = get_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = delta} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
         {Dicts1, #delta{
               dictionary = DictName,
               key = get_attribute(key, XmlElem),
               ns = get_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
       XmlElem = #xmlElement{name = tail} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dict:new_dict(DictName, Dicts),
         {Dicts1, #tail{
               dictionary = DictName,
               key = get_attribute(key, XmlElem),
               ns = get_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}}
      end.

need_pmap("mandatory") -> false;
need_pmap("optional") -> true.

need_pmap("mandatory", #constant{}) ->
   false;
need_pmap("optional", #constant{}) ->
   true;
need_pmap(_, undef) ->
   false;
need_pmap(_, Op) when is_record(Op, default) or is_record(Op, copy) or is_record(Op, increment) ->
   true;
need_pmap(_, #delta{}) ->
   false;
need_pmap(_, #tail{}) ->
   true;
need_pmap(Presence, {decFieldOp, ExpOp, MantOp}) ->
   need_pmap(Presence, ExpOp) or need_pmap(Presence, MantOp).

%% ====================================================================================================================
%% conversion tools
%% ====================================================================================================================

string_to_dic(undef) ->
   undef;
string_to_dic(Str) when (Str =:= "template") or (Str =:= "type") or (Str =:= "global") ->
   erlang:list_to_atom(Str);
string_to_dic(Str) ->
   Str.

string_to_presence("optional") ->
   optional;
string_to_presence("mandatory") ->
   mandatory;
string_to_presence(Str) ->
   erlang:error({unkown_presence_attribute, Str}).

string_to_charset("ascii") ->
   ascii;
string_to_charset("unicode") ->
   unicode;
string_to_charset(Str) ->
   erlang:error({unknown_charset_attribute, Str}).

string_to_id(undef) ->
   undef;
string_to_id(Str) ->
   erlang:list_to_integer(Str).

string_to_type(_Type, undef) ->
   undef;
string_to_type(Type, Str)
when (Type =:= int32) or (Type =:= 'Int64') or (Type =:= uInt32) or (Type =:= uInt64) or (Type =:= length) ->
   erlang:list_to_integer(Str);
string_to_type(decimal, Str) ->
   case re:split(Str, "[.]", [{return, list}]) of
      [Num] ->
         {list_to_integer(Num), 0};
      [[], Remainder] ->
         {list_to_integer(Remainder), -length(Remainder)};
      [Num, []] ->
         {erlang:list_to_integer(Num), 0};
      [Num, Remainder] ->
         {list_to_integer(Num) * round(math:pow(10, length(Remainder))) + list_to_integer(Remainder), -length(Remainder)}
   end;
string_to_type(byteVector, Str) ->
   Bytes = string:tokens(Str, " "),
   lists:foldl(fun(Byte, Acc) -> Int = erlang:list_to_integer(Byte, 16), <<Acc/binary, Int/integer>> end, <<>>, Bytes);
string_to_type(_Type, Str) ->
   Str.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
   {_Dicts, {templates, _, _, _, _Templates}} = parse("doc/templates.xml").

string_to_type_test() ->
   ?assertEqual(100123, string_to_type(int32, "100123")),
   ?assertEqual({1, 0}, string_to_type(decimal, "1.")),
   ?assertEqual({0, -1}, string_to_type(decimal, ".0")),
   ?assertEqual({10, 0}, string_to_type(decimal, "10")),
   ?assertEqual({1, -1}, string_to_type(decimal, ".1")),
   ?assertEqual({1123, -4}, string_to_type(decimal, ".1123")),
   ?assertEqual({12345, -4}, string_to_type(decimal, "1.2345")),
   ?assertEqual(<<160, 239, 18, 186>>, string_to_type(byteVector, "a0 ef 12 ba")).

-endif.
