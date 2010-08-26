% some helpful utilities for xml manipulation

-module(erlang_fast_xml_tools).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-export([
         get_tag/2
         ,get_text/1
         ,get_text/2
         ,get_attribute/2
         ,get_attribute/3
        ]).

-include_lib("xmerl/include/xmerl.hrl").

%get_tag(Path, XmlElement) -> throw(not_found) | throw(error_multiple_tags) | XmlElement | [XmlElement]
%  Path = [Tag],
%  Tag = atom()
%  XmlElement = #xmlElement
%  Return:
%     XmlElement - found XML tag
%     [XmlElement] - multiple XML tags found
%  Throw:
%     not_found - no tag found
%     error_multiple_tags - one of Path element (except last) has multiple #xmlElement
get_tag([], XmlElem = #xmlElement{}) ->
   XmlElem;

get_tag([TagName | _], #xmlElement{name = Tag, content = _Childs}) when TagName =/= Tag ->
   {error_not_found, TagName};

get_tag([TagName], XmlElem = #xmlElement{name = TagName}) ->
   XmlElem;

get_tag([TagName | Rest], #xmlElement{name = TagName, content = Childs}) ->
   get_tag_aux(Rest, Childs).

%get_text(XmlElement) -> [] | String()
%  XmlElement = #xmlElement
get_text(#xmlElement{content = Childs}) ->
   case lists:keyfind(xmlText, 1, Childs) of
      false ->
         [];
      #xmlText{value = Text} ->
         Text
   end.

%get_text(XmlElement, DefVal) -> [] | String()
%  XmlElement = #xmlElement
%  DefVal = term()
get_text(#xmlElement{content = Childs}, DefVal) ->
   case lists:keyfind(xmlText, 1, Childs) of
      false ->
         DefVal;
      #xmlText{value = Text} ->
         Text
   end.

%get_attribute(Name, XmlElement) -> [] | String()
%  Name  = atom(), mane of attribute
%  XmlElement = #xmlElement
get_attribute(Name, #xmlElement{attributes = Attributes}) ->
   case lists:keyfind(Name, 2, Attributes) of
      false ->
         undef;
      #xmlAttribute{value = Text} ->
         Text
   end.

%get_attribute(Name, XmlElement, DefVal) -> [] | String()
%  Name  = atom(), mane of attribute
%  XmlElement = #xmlElement
%  DefVal = term()
get_attribute(Name, #xmlElement{attributes = Attributes}, DefVal) ->
   case lists:keyfind(Name, 2, Attributes) of
      false ->
         DefVal;
      #xmlAttribute{value = Text} ->
         Text
   end.

get_tag_aux([TagName | Rest], Childs) ->
   Res = lists:filter(fun(#xmlElement{name = Tag}) when Tag =:= TagName -> true;
                         (_) -> false end, Childs),
   case Res of
      Tags when (erlang:length(Tags) > 1) and (erlang:length(Rest) > 0) ->
         {error_multiple_tags, TagName};
      Tags when (erlang:length(Tags) > 1) and (erlang:length(Rest) =:= 0) ->
         Tags;
      [] ->
         {error_not_found, TagName};
      [XmlElem] when erlang:length(Rest) =:= 0 ->
         XmlElem;
      [#xmlElement{content = ChildTags}] ->
         get_tag_aux(Rest, ChildTags)
   end.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_tag_test() ->
   Xml = "<root><tag>text</tag><tag1>text1</tag1></root>",
   {XmlElement, _} = xmerl_scan:string(Xml),
   ?assertMatch(#xmlElement{name = tag1}, get_tag([root, tag1], XmlElement)),
   Xml1 = "<root><tag>text</tag><tag>text</tag></root>",
   {XmlElement1, _} = xmerl_scan:string(Xml1),
   ?assertMatch([#xmlElement{name = tag}, #xmlElement{name = tag}], get_tag([root, tag], XmlElement1)),
   ?assertEqual({error_not_found, root1}, get_tag([root1, tag], XmlElement1)),
   ?assertEqual({error_not_found, tag1}, get_tag([root, tag1], XmlElement1)),
   Xml2 = "<root><tag></tag><tag><tag1></tag1></tag></root>",
   {XmlElement2, _} = xmerl_scan:string(Xml2),
   ?assertEqual({error_multiple_tags, tag}, get_tag([root, tag, tag1], XmlElement2)),
   ?assertMatch(#xmlElement{name = root}, get_tag([root], XmlElement2)),
   ?assertEqual({error_not_found, root1}, get_tag([root1], XmlElement2)).

get_attribute_test() ->
   Xml = "<root><tag attr=\"attr_text\">text</tag></root>",
   {XmlElement, _} = xmerl_scan:string(Xml),
   ?assertEqual("attr_text", get_attribute(attr, get_tag([root, tag], XmlElement))),
   ?assertEqual(undef, get_attribute(attr1, get_tag([root, tag], XmlElement))).

get_text_test() ->
   Xml = "<root><tag attr=\"attr_text\">text</tag></root>",
   {XmlElement, _} = xmerl_scan:string(Xml),
   ?assertEqual("text", get_text(get_tag([root, tag], XmlElement))).

-endif.
