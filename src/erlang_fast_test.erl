-module(erlang_fast_test).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(xmlDescr,
   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
    <templates xmlns=\"http://www.fixprotocol.org/ns/fast/td/1.1\">
    <template dictionary=\"1\" id=\"1\" name=\"MDIncRefresh\">
      <string id=\"1128\" name=\"ApplVerID\">
         <constant value=\"13\"/>
      </string>
      <string id=\"35\" name=\"MessageType\">
         <constant value=\"X\"/>
      </string>
      <string id=\"49\" name=\"SenderCompID\">
         <constant value=\"XXXXX\"/>
      </string>
      <uInt32 id=\"34\" name=\"MsgSeqNum\"/>
      <uInt64 id=\"52\" name=\"SendingTime\"/>
      <string id=\"43\" name=\"PosDupFlag\" presence=\"optional\">
         <default/>
      </string>
      <uInt32 id=\"75\" name=\"TradeDate\"/>
      <sequence name=\"MDEntries\">
         <length id=\"268\" name=\"NoMDEntries\"/>
         <uInt32 id=\"279\" name=\"MDUpdateAction\">
            <copy value=\"1\"/>
         </uInt32>
         <uInt32 id=\"1023\" name=\"MDPriceLevel\">
            <increment value=\"1\"/>
         </uInt32>
         <string id=\"269\" name=\"MDEntryType\">
            <copy value=\"0\"/>
         </string>
         <uInt32 id=\"273\" name=\"MDEntryTime\">
            <copy/>
         </uInt32>
         <uInt32 id=\"22\" name=\"SecurityIDSource\">
            <constant value=\"8\"/>
         </uInt32>
         <uInt32 id=\"48\" name=\"SecurityID\">
            <copy/>
         </uInt32>
         <uInt32 id=\"83\" name=\"RptSeq\">
            <increment/>
         </uInt32>
         <decimal id=\"270\" name=\"MDEntryPx\">
            <exponent>
               <default value=\"0\"/>
            </exponent>
            <mantissa>
               <delta/>
            </mantissa>
         </decimal>
         <int32 id=\"271\" name=\"MDEntrySize\">
            <delta/>
         </int32>
         <uInt32 id=\"346\" name=\"NumberOfOrders\">
            <delta/>
         </uInt32>
         <string id=\"336\" name=\"TradingSessionID\">
            <default value=\"2\"/>
         </string>
      </sequence>
   </template>
   </templates>").

-define(msg, {1,
       [{"ApplVerID",<<"13">>},
        {"MessageType",<<"X">>},
        {"SenderCompID",<<"XXXXX">>},
        {"MsgSeqNum",3033666},
        {"SendingTime",20100713060000051},
        {"TradeDate",20100713},
        {"MDEntries",
         [[{"MDUpdateAction",1},
           {"MDPriceLevel",2},
           {"MDEntryType",<<"0">>},
           {"MDEntryTime",60000000},
           {"SecurityIDSource",8},
           {"SecurityID",26714},
           {"RptSeq",5049472},
           {"MDEntryPx",{107425,0}},
           {"MDEntrySize",189},
           {"NumberOfOrders",30},
           {"TradingSessionID",<<"2">>}],
          [{"MDUpdateAction",1},
           {"MDPriceLevel",3},
           {"MDEntryType",<<"0">>},
           {"MDEntryTime",60000000},
           {"SecurityIDSource",8},
           {"SecurityID",26714},
           {"RptSeq",5049473},
           {"MDEntryPx",{1074,2}},
           {"MDEntrySize",246},
           {"NumberOfOrders",34},
           {"TradingSessionID",<<"2">>}],
          [{"MDUpdateAction",1},
           {"MDPriceLevel",1},
           {"MDEntryType",<<"1">>},
           {"MDEntryTime",60000000},
           {"SecurityIDSource",8},
           {"SecurityID",26714},
           {"RptSeq",5049474},
           {"MDEntryPx",{107475,0}},
           {"MDEntrySize",72},
           {"NumberOfOrders",18},
           {"TradingSessionID",<<"2">>}]]}]}).

-define(data, <<192, 129, 1, 57, 20, 194, 35, 90, 47, 95, 45, 49, 66, 179, 9, 74, 108, 233, 131, 174, 130, 28, 78, 14, 128,
   1, 80, 218, 2, 52, 25, 128, 6, 71, 161, 1, 189, 158, 129, 130, 121, 65, 145, 185, 132, 176, 129, 177, 6, 63, 161,
   126, 210, 240, 128, 1, 57, 20, 195, 35, 90, 47, 95, 45, 49, 66, 180, 9, 74, 108, 233, 129, 177, 129, 176, 129, 122,
   12, 166, 151, 250, 192, 212, 1, 57, 20, 196, 35, 90, 47, 95, 45, 49, 66, 195, 9, 74, 108, 233, 129, 30, 176, 1, 80,
   218, 2, 52, 25, 132, 129, 0, 83, 249, 134, 131, 254, 3, 46, 144, 28, 78, 14, 128, 131, 192, 237, 1, 57, 20, 197, 35,
   90, 47, 95, 45, 49, 66, 195, 9, 74, 108, 233, 130, 45, 224, 130, 1, 80, 218, 2, 52, 25, 133, 129, 0, 83, 249, 141,
   28, 78, 14, 128, 0, 219, 128, 17, 192, 177, 130, 127, 52, 186, 158, 0, 198, 128, 192, 211, 1, 57, 20, 198, 35, 90,
   47, 95, 45, 49, 66, 200, 9, 74, 108, 233, 130, 147, 177, 2, 52, 25, 135, 130, 127, 52, 186, 190, 144, 128, 6, 63,
   210, 134, 136, 128, 1, 57, 20, 199, 35, 90, 47, 95, 45, 49, 66, 200, 9, 74, 108, 233, 130, 160, 131, 128, 254, 255,
   129>>).

decode_test() ->
   Context = erlang_fast:create_context(?xmlDescr, fun logger/2),
   {Msg, _, _} = erlang_fast:decode(?data, Context),
   ?assertEqual(?msg, Msg).

encode_test() ->
   ok.
   %Context = erlang_fast:create_context(?xmlDescr, fun logger/2),
   %{Data, _} = erlang_fast:encode(?msg, Context),
   %?debugFmt("~p", [Data]).

find_template_test() ->
   Context = erlang_fast:create_context(?xmlDescr, fun logger/2),
   ?assertMatch({template, "MDIncRefresh", _, 1, _, "1", _}, erlang_fast_templates:get_by_id(1,
         Context#context.templates#templates.tlist)).

logger([], _) ->
   ok;
logger(Err, Value) ->
   io:format("~p: ~p~n", [Err, Value]).
