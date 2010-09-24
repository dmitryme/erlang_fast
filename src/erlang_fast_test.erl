-module(erlang_fast_test).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
   Data = <<
   16#c0, 16#81, 16#01, 16#39, 16#14, 16#c2, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#b3, 16#09, 16#4a,
   16#6c, 16#e9, 16#83, 16#ae, 16#82, 16#1c, 16#4e, 16#0e, 16#80, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#80,
   16#06, 16#47, 16#a1, 16#01, 16#bd, 16#9e, 16#81, 16#82, 16#79, 16#41, 16#91, 16#b9, 16#84, 16#b0, 16#81, 16#b1,
   16#06, 16#3f, 16#a1, 16#7e, 16#d2, 16#f0, 16#80, 16#01, 16#39, 16#14, 16#c3, 16#23, 16#5a, 16#2f, 16#5f, 16#2d,
   16#31, 16#42, 16#b4, 16#09, 16#4a, 16#6c, 16#e9, 16#81, 16#b1, 16#81, 16#b0, 16#81, 16#7a, 16#0c, 16#a6, 16#97,
   16#fa, 16#c0, 16#d4, 16#01, 16#39, 16#14, 16#c4, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09,
   16#4a, 16#6c, 16#e9, 16#81, 16#1e, 16#b0, 16#01, 16#50, 16#da, 16#02, 16#34, 16#19, 16#84, 16#81, 16#00, 16#53,
   16#f9, 16#86, 16#83, 16#fe, 16#03, 16#2e, 16#90, 16#1c, 16#4e, 16#0e, 16#80, 16#83, 16#c0, 16#ed, 16#01, 16#39,
   16#14, 16#c5, 16#23, 16#5a, 16#2f, 16#5f, 16#2d, 16#31, 16#42, 16#c3, 16#09, 16#4a, 16#6c, 16#e9, 16#82, 16#2d>>,
   Context = erlang_fast:create_context("doc/template.xml", fun logger/2),
   {Msg, _, _} = erlang_fast:decode(Data, Context),
   ?debugFmt("~p", [Msg]).

%encode_test() ->
%   Msg = {1,
%    [{"ApplVerID",<<"9">>},
%     {"MessageType",<<"X">>},
%     {"SenderCompID",<<"CME">>},
%     {"MsgSeqNum",3033666},
%     {"SendingTime",20100713060000051},
%     {"TradeDate",20100713},
%     {"MDEntries",
%      [[{"MDUpdateAction",1},
%        {"MDPriceLevel",2},
%        {"MDEntryType",<<"0">>},
%        {"MDEntryTime",60000000},
%        {"SecurityIDSource",8},
%        {"SecurityID",26714},
%        {"RptSeq",5049472},
%        {"MDEntryPx",{107425,0}},
%        {"MDEntrySize",189},
%        {"NumberOfOrders",30},
%        {"TradingSessionID",<<"2">>}],
%       [{"MDUpdateAction",1},
%        {"MDPriceLevel",3},
%        {"MDEntryType",<<"0">>},
%        {"MDEntryTime",60000000},
%        {"SecurityIDSource",8},
%        {"SecurityID",26714},
%        {"RptSeq",5049473},
%        {"MDEntryPx",{1074,2}},
%        {"MDEntrySize",246},
%        {"NumberOfOrders",34},
%        {"TradingSessionID",<<"2">>}],
%       [{"MDUpdateAction",1},
%        {"MDPriceLevel",1},
%        {"MDEntryType",<<"1">>},
%        {"MDEntryTime",60000000},
%        {"SecurityIDSource",8},
%        {"SecurityID",26714},
%        {"RptSeq",5049474},
%        {"MDEntryPx",{107475,0}},
%        {"MDEntrySize",72},
%        {"NumberOfOrders",18},
%        {"TradingSessionID",<<"2">>}]]}]},
%   Context = erlang_fast:create_context("doc/template.xml", fun logger/2),
%   {Data, _} = erlang_fast:encode(Msg, Context),
%   ?debugFmt("~p", [Data]).

logger([], _) ->
   ok;
logger(Err, Value) ->
   io:format("~p: ~p~n", [Err, Value]).
