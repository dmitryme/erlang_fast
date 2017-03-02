-module(erlang_fast_xml).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-compile({no_auto_import,[list_to_integer/1, length/1]}).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlang_fast_template.hrl").
-include("erlang_fast_common.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(erlang_fast_xml_utils, [get_attribute/2, get_attribute/3, get_bin_attribute/2, get_bin_attribute/3]).

-export(
   [
      parse/2
   ]).

parse({file, XmlFile}, Options) ->
   {RootElem, []} = xmerl_scan:file(XmlFile),
   init(RootElem, Options);

parse(XmlText, Options) ->
   {RootElem, []} = xmerl_scan:string(XmlText),
   init(RootElem, Options).

init(RootElem, Options) ->
   Dicts = erlang_fast_dicts:init(),
   DictName = string_to_dic(get_attribute(dictionary, RootElem, global)),
   Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
   Dicts2 = erlang_fast_dicts:new_dict(?type_dictionary(<<"any">>), Dicts1),
   {Dicts3, TList} = parse_template(RootElem, Dicts2, DictName, Options, erlang_fast_templates:init()),
   {Dicts3, #templates{
         ns = get_bin_attribute(ns, RootElem),
         templateNs = get_bin_attribute(templateNs, RootElem),
         dictionary = DictName,
         tlist = TList}}.

parse_template([], Dicts, _DefDict, _Options, Templates) ->
   {Dicts, Templates};

parse_template(#xmlElement{name = templates, content = Childs}, Dicts, DefDict, Options, Templates) ->
   parse_template(Childs, Dicts, DefDict, Options, Templates);

parse_template([XmlElem = #xmlElement{content = Childs} | Rest], Dicts, DefDict, Options, Templates) ->
   TemplateName = get_bin_attribute(name, XmlElem),
   DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
   Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
   Dicts2 = erlang_fast_dicts:new_dict(?template_dictionary(TemplateName), Dicts1),
   {Dicts3, Instructions, _} = parse_instruction(Childs, Dicts2, DictName, Options),
   Template = #template{
                  name = get_bin_attribute(name, XmlElem),
                  templateNs = get_bin_attribute(templateNs, XmlElem),
                  id = string_to_id(get_attribute(id, XmlElem)),
                  ns = get_bin_attribute(ns, XmlElem),
                  dictionary = DictName,
                  instructions = Instructions},
   Templates1 = erlang_fast_templates:add_template(Template, Templates),
   parse_template(Rest, Dicts3, DefDict, Options, Templates1);

parse_template([#xmlText{} | Rest], Dicts, DefDict, Options, Templates) ->
   parse_template(Rest, Dicts, DefDict, Options, Templates);

parse_template([#xmlComment{} | Rest], Dicts, DefDict, Options, Templates) ->
   parse_template(Rest, Dicts, DefDict, Options, Templates).

parse_instruction([], Dicts, _DefDict, _Options) ->
   {Dicts, [], false};

parse_instruction([I = #xmlElement{name = typeRef} | Tail], Dicts, DefDict, Options) ->
   TypeName = get_attribute(name, I),
   Dicts1 = erlang_fast_dicts:new_dict(?type_dictionary(TypeName), Dicts),
   {Dicts2, Instructions, NeedPMap} = parse_instruction(Tail, Dicts1, DefDict, Options),
   {Dicts2, [#typeRef{
            name = get_bin_attribute(name, I),
            ns = get_bin_attribute(ns, I)} | Instructions], NeedPMap or false};

parse_instruction([I = #xmlElement{name = templateRef} | Tail], Dicts, DefDict, Options) ->
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict, Options),
   {Dicts1, [#templateRef{
            name = get_bin_attribute(name, I),
            templateNs = get_bin_attribute(templateNs, I),
            ns = get_bin_attribute(ns, I)} | Instructions], NeedPMap or false};

parse_instruction([I = #xmlElement{name = string = T, content = Childs} | Tail], Dicts, DefDict, Options) ->
   OpName = get_bin_attribute(name, I),
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict, Options),
   {Dicts2, Operator} = parse_op(OpName, T, Childs, Dicts1, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   Instr =  case get_attribute(charset, I, "ascii") of
               "ascii" ->
                  #field{
                     type = string,
                     name = OpName,
                     disp_name = get_disp_name(Options, OpName, string_to_id(get_attribute(id, I))),
                     ns = get_bin_attribute(ns, I),
                     id = string_to_id(get_attribute(id, I)),
                     presence = string_to_presence(Presence),
                     operator = Operator};
               "unicode" ->
                  #field{
                     type = unicode,
                     name = OpName,
                     disp_name = get_disp_name(Options, OpName, string_to_id(get_attribute(id, I))),
                     ns = get_bin_attribute(ns, I),
                     id = string_to_id(get_attribute(id, I)),
                     presence = string_to_presence(Presence),
                     operator = Operator}
            end,
   {Dicts2, [Instr | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = Type, content = Childs} | Tail], Dicts, DefDict, Options)
  when (Type == int32 ) or (Type == uInt32) or (Type == uInt64) or (Type == 'int64') or (Type == byteVector) or
       (Type == decimal)->
   OpName = get_bin_attribute(name, I),
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict, Options),
   {Dicts2, Operator} =
   if
      (Type == decimal) -> parse_dec_op(OpName, Childs, Dicts1, DefDict);
      true -> parse_op(OpName, Type, Childs, Dicts1, DefDict)
   end,
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts2, [#field{
            type = if Type == 'Int64' -> int64; true -> Type end,
            name = OpName,
            disp_name = get_disp_name(Options, OpName, string_to_id(get_attribute(id, I))),
            ns = get_bin_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            operator = Operator} | Instructions], NeedPMap or need_pmap(Presence, Operator)};

parse_instruction([I = #xmlElement{name = length = T, content = Childs} | Tail], Dicts, DefDict, Options) ->
   OpName = get_bin_attribute(name, I),
   {Dicts1, Instructions, NeedPMap} = parse_instruction(Tail, Dicts, DefDict, Options),
   {Dicts2, Operator} = parse_op(OpName, T, Childs, Dicts1, DefDict),
   {Dicts2, [#field{
               type = T,
               name = OpName,
               disp_name = get_disp_name(Options, OpName, string_to_id(get_attribute(id, I))),
               ns = get_bin_attribute(ns, I),
               id = string_to_id(get_attribute(id, I)),
               operator = Operator} | Instructions], NeedPMap or false};

parse_instruction([I = #xmlElement{name = Type, content = Childs} | Tail], Dicts, DefDict, Options)
  when (Type == sequence) or (Type == group) ->
   DictName = string_to_dic(get_attribute(dictionary, I, DefDict)),
   {Dicts1, Length, NeedPMap, Childs1} = parse_length(Childs, Dicts, DictName, Options),
   {Dicts2, GroupInstructions, NeedPMap1} = parse_instruction(Childs1, Dicts1, DictName, Options),
   {Dicts3, Instructions, _NeedPMap} = parse_instruction(Tail, Dicts2, DictName,  Options),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts3, [#field_group{
            type = Type,
            name = get_bin_attribute(name, I),
            disp_name = get_disp_name(Options, GroupInstructions, get_bin_attribute(name, I), string_to_id(get_attribute(id, I))),
            ns = get_attribute(ns, I),
            id = string_to_id(get_attribute(id, I)),
            presence = string_to_presence(Presence),
            dictionary = DictName,
            need_pmap = NeedPMap1,
            instructions = [Length | GroupInstructions]} | Instructions], NeedPMap};

parse_instruction([#xmlText{} | Tail], Dicts, DefDict, Options) ->
   parse_instruction(Tail, Dicts, DefDict, Options);


% Handle XML comments in template file
parse_instruction([#xmlComment{} | Tail], Dicts, DefDict, Options) ->
  %just ignore them...
  parse_instruction(Tail, Dicts, DefDict, Options);


parse_instruction([I | _Tail], _, _, _) ->
   erlang:error({unknown_tag, I}).

parse_length([], Dicts, _DefDict, _Options) ->
   OpName = <<"Length">>,
   Operator = undef,
   Presence = "optional",
   {Dicts, #field{
               type = uInt32,
               name = OpName,
               disp_name = <<"Length">>,
               ns = undef,
               id = undef,
               operator = Operator}, need_pmap(Presence, Operator), []};

parse_length([I = #xmlElement{name = length, content = Childs} | Tail], Dicts, DefDict, Options) ->
   OpName = get_bin_attribute(name, I),
   {Dicts1, Operator} = parse_op(OpName, length, Childs, Dicts, DefDict),
   Presence = get_attribute(presence, I, "mandatory"),
   {Dicts1, #field{
               type = uInt32,
               name = OpName,
               disp_name = get_disp_name(Options, OpName, string_to_id(get_attribute(id, I))),
               ns = get_bin_attribute(ns, I),
               id = string_to_id(get_attribute(id, I)),
               operator = Operator}, need_pmap(Presence, Operator), Tail};

parse_length([#xmlElement{} | _Tail], Dicts, DefDict, Options) ->
   parse_length([], Dicts, DefDict, Options);

parse_length([_ | Tail], Dicts, DefDict, Options) ->
   parse_length(Tail, Dicts, DefDict, Options).

parse_dec_op(OpName, Childs, Dicts, DefDict) ->
   Res = lists:foldr(fun(#xmlElement{name = exponent, content = C}, {D, DecFieldOp}) ->
            {Dicts1, Operator} = parse_op(<<OpName/binary, <<"_exponent">>/binary>>, int32, C, D, DefDict),
            {Dicts1, DecFieldOp#decFieldOp{exponent = Operator}};
         (#xmlElement{name = mantissa, content = C}, {D, DecFieldOp}) ->
            {Dicts1, Operator} = parse_op(<<OpName/binary,  <<"_mantissa">>/binary>>, int64, C, D, DefDict),
            {Dicts1, DecFieldOp#decFieldOp{mantissa = Operator}};
         (_, Acc) ->
            Acc end,
      {Dicts, #decFieldOp{exponent = undef, mantissa = undef}}, Childs),
   case Res of
      {Dicts, #decFieldOp{exponent = undef, mantissa = undef}} ->
         parse_op(OpName, decimal, Childs, Dicts, DefDict);
      _ ->
         Res
   end.

parse_op(OpName, Type, Childs, Dicts, DefDict) ->
   case lists:keyfind(xmlElement, 1, Childs) of
      false ->
         {Dicts, undef};
      XmlElem = #xmlElement{name = constant} ->
         case get_attribute(value, XmlElem) of
            undef ->
               throw({error, {'ERR S4', "Constant operator doesn't have an initial value"}});
            Value ->
               {Dicts, #constant{value = string_to_type(Type, Value)}}
         end;
      XmlElem = #xmlElement{name = default} ->
         {Dicts, #default{value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = copy} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
         {Dicts1, #copy{
               dictionary = DictName,
               key = get_attribute(key, XmlElem, OpName),
               ns = get_bin_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = increment} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
         {Dicts1, #increment{
               dictionary = DictName,
               key = get_attribute(key, XmlElem, OpName),
               ns = get_bin_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
      XmlElem = #xmlElement{name = delta} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
         {Dicts1, #delta{
               dictionary = DictName,
               key = get_attribute(key, XmlElem, OpName),
               ns = get_bin_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}};
       XmlElem = #xmlElement{name = tail} ->
         DictName = string_to_dic(get_attribute(dictionary, XmlElem, DefDict)),
         Dicts1 = erlang_fast_dicts:new_dict(DictName, Dicts),
         {Dicts1, #tail{
               dictionary = DictName,
               key = get_attribute(key, XmlElem, OpName),
               ns = get_bin_attribute(ns, XmlElem),
               value = string_to_type(Type, get_attribute(value, XmlElem))}}
      end.

%% ====================================================================================================================
%% helpers
%% ====================================================================================================================
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

string_to_id(undef) ->
   undef;
string_to_id(Str) ->
   erlang:list_to_integer(Str).

string_to_type(_Type, undef) ->
   undef;
string_to_type(Type, Str)
when (Type =:= int32) or (Type =:= 'Int64') or (Type =:= uInt32) or (Type =:= uInt64) or (Type =:= length) ->
   try
      erlang:list_to_integer(Str)
   catch
      _:_ ->
         Reason = list:flatten(io_lib:format("Unable to convert ~p to number", [Str])),
         throw({error, 'ERR S3', Reason})
   end;
string_to_type(decimal, Str) ->
   try
      erlang_fast_utils:str_to_decimal(Str)
   catch
      _:_ ->
         Reason = lists:flatten(io_lib:format("Unable to convert ~p to decimal", [Str])),
         throw({error, 'ERR S3', Reason})
   end;
string_to_type(Type, Str) when Type == byteVector orelse Type == string ->
   erlang:list_to_binary(Str).

get_disp_name(Options, Name, Id) when is_list(Options) ->
   get_disp_name(proplists:get_value(use_id, Options, false), Name, Id);

get_disp_name(false, Name, _Id) ->
   Name;
get_disp_name(true, Name, undef) ->
   Name;
get_disp_name(true, _Name, Id) ->
   Id.

get_disp_name(Options, GroupInstructions, Name, Id) when is_list(Options) ->
   get_disp_name(proplists:get_value(use_id, Options, false), GroupInstructions, Name, Id);

get_disp_name(false, _GroupInstructions, Name, _Id) ->
   Name;
get_disp_name(true, [#field{type = length, disp_name = DispName}|_Rest], _Name, undef) ->
   DispName;
get_disp_name(true, _GroupInstructions, Name, undef) ->
   Name;
get_disp_name(true, _GroupInstructions, _Name, Id) ->
   Id.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(TEST).

string_to_type_test() ->
   ?assertEqual(100123, string_to_type(int32, "100123")),
   ?assertEqual({1012, -3}, string_to_type(decimal, "1.0120")),
   ?assertEqual({-1012, -3}, string_to_type(decimal, "-001.012")),
   ?assertEqual({21, 0}, string_to_type(decimal, "21.0")),
   ?assertEqual({12, 3}, string_to_type(decimal, "12000")),
   ?assertEqual({-12, 3}, string_to_type(decimal, "-12000")),
   ?assertEqual({1, 0}, string_to_type(decimal, "1.")),
   ?assertEqual({0, 0}, string_to_type(decimal, ".0")),
   ?assertEqual({0, 0}, string_to_type(decimal, "0.0")),
   ?assertEqual({0, 0}, string_to_type(decimal, "00.0000")),
   ?assertEqual({1, 1}, string_to_type(decimal, "10")),
   ?assertEqual({1, -1}, string_to_type(decimal, ".1")),
   ?assertEqual({1123, -4}, string_to_type(decimal, ".1123")),
   ?assertEqual({12345, -4}, string_to_type(decimal, "1.2345")),
   ?assertEqual(<<"1.2345">>, string_to_type(string, "1.2345")),
   ?assertEqual(<<"FOND">>, string_to_type(byteVector, "FOND")).

-endif.
