-module(erlang_fast_xml).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-compile([export_all]).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlang_fast_template.hrl").

-import(erlang_fast_xml_utils, [get_attribute/2, get_attribute/3]).

parse(XmlFile) ->
   {RootElem, []} = xmerl_scan:file(XmlFile),
   #templates{
      ns = get_attribute(ns, RootElem),
      templateNs = get_attribute(templateNs, RootElem),
      dictionary = string_to_dic(string_to_dic(get_attribute(dictionary, RootElem))),
      tlist = parse_template(RootElem)}.

parse_template([]) ->
   [];

parse_template(#xmlElement{name = templates, content = Childs}) ->
   parse_template(Childs);

parse_template([XmlElem = #xmlElement{content = Childs} | Rest]) ->
   [#template{
         name = get_attribute(name, XmlElem),
         templateNs = get_attribute(templateNs, XmlElem),
         id = string_to_id(get_attribute(id, XmlElem)),
         ns = get_attribute(ns, XmlElem),
         dictionary = get_attribute(dictionary, XmlElem),
         typeRef = get_attribute(typeRef, XmlElem),
         instructions = parse_instruction(Childs)} | parse_template(Rest)];

parse_template([#xmlText{} | Rest]) ->
      parse_template(Rest).

parse_instruction([]) ->
   [];
parse_instruction([I = #xmlElement{name = string = T, content = Childs} | Tail]) ->
   [#string{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         charset = get_attribute(charset, I, ascii),
         length = get_attribute(length, I),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = int32 = T, content = Childs} | Tail]) ->
   [#int32{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = 'Int64' = T, content = Childs} | Tail]) ->
   [#int64{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = uInt32 = T, content = Childs} | Tail]) ->
   [#uInt32{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = uInt64 = T, content = Childs} | Tail]) ->
   [#uInt64{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = length = T, content = Childs} | Tail]) ->
   [#length{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         operator = parse_op(T, Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = decimal, content = Childs} | Tail]) ->
   [#decimal{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_dec_op(Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = byteVector, content = Childs} | Tail]) ->
   [#decimal{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         operator = parse_instruction(Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = sequence, content = Childs} | Tail]) ->
   [#sequence{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         dictionary = get_attribute(dictionary, I),
         typeRef = get_attribute(typeRef, I),
         instructions = parse_instruction(Childs)} | parse_instruction(Tail)];

parse_instruction([I = #xmlElement{name = group, content = Childs} | Tail]) ->
   [#group{
         name = get_attribute(name, I),
         ns = get_attribute(ns, I),
         id = string_to_id(get_attribute(id, I)),
         presence = string_to_presence(get_attribute(presence, I, "mandatory")),
         dictionary = get_attribute(dictionary, I),
         typeRef = get_attribute(typeRef, I),
         instructions = parse_instruction(Childs)} | parse_instruction(Tail)];

parse_instruction([#xmlText{} | Tail]) ->
   parse_instruction(Tail);

parse_instruction([I | _Tail]) ->
   erlang:error({unknown_tag, I}).

parse_dec_op(Childs) ->
   Res = lists:foldr(fun(#xmlElement{name = exponent, content = C}, DecFieldOp) ->
                           DecFieldOp#decFieldOp{exponent = parse_op(decimal, C)};
                         (#xmlElement{name = mantissa, content = C}, DecFieldOp) ->
                           DecFieldOp#decFieldOp{mantissa = parse_op(decimal, C)};
                         (_, DecFieldOp) ->
                           DecFieldOp end,
               #decFieldOp{exponent = undef, mantissa = undef}, Childs),
   case Res of
      #decFieldOp{exponent = undef, mantissa = undef} ->
         parse_op(decimal, Childs);
      _ ->
         Res
   end.

parse_op(Type, Childs) ->
   case lists:keyfind(xmlElement, 1, Childs) of
      false ->
         undef;
      XmlElem = #xmlElement{name = constant} ->
         #constant{value = string_to_type(Type, get_attribute(value, XmlElem))};
      XmlElem = #xmlElement{name = default} ->
         #default{value = string_to_type(Type, get_attribute(value, XmlElem))};
      XmlElem = #xmlElement{name = copy} ->
         #copy{
            dictionary = get_attribute(dictionary, XmlElem),
            key = get_attribute(key, XmlElem),
            ns = get_attribute(ns, XmlElem),
            value = string_to_type(Type, get_attribute(value, XmlElem))};
      XmlElem = #xmlElement{name = increment} ->
         #increment{
            dictionary = get_attribute(dictionary, XmlElem),
            key = get_attribute(key, XmlElem),
            ns = get_attribute(ns, XmlElem),
            value = string_to_type(Type, get_attribute(value, XmlElem))};
      XmlElem = #xmlElement{name = delta} ->
         #delta{
            dictionary = get_attribute(dictionary, XmlElem),
            key = get_attribute(key, XmlElem),
            ns = get_attribute(ns, XmlElem),
            value = string_to_type(Type, get_attribute(value, XmlElem))};
      XmlElem = #xmlElement{name = tail} ->
         #tail{
            dictionary = get_attribute(dictionary, XmlElem),
            key = get_attribute(key, XmlElem),
            ns = get_attribute(ns, XmlElem),
            value = string_to_type(Type, get_attribute(value, XmlElem))}
   end.

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
   when (Type =:= int32) or (Type =:= 'Int64') or (Type =:= uInt32) or (Type =:= uInt64) or (Type =:= length) or (Type =:= decimal)->
   erlang:list_to_integer(Str);
string_to_type(_Type, Str) ->
   Str.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
   {templates, _, _, _, _Templates} = parse("doc/templates.xml").

-endif.
