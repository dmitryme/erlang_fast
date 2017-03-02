-module(erlang_fast_test).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(xmlDescr,
   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
    <templates xmlns=\"http://www.fixprotocol.org/ns/fast/td/1.1\">
    <template dictionary=\"1\" id=\"1\" name=\"MDIncRefresh\">
      <string id=\"1128\" name=\"ApplVerID\"> <constant value=\"13\"/> </string>
      <string id=\"35\" name=\"MessageType\"> <constant value=\"X\"/> </string>
      <string id=\"49\" name=\"SenderCompID\"> <constant value=\"XXXXX\"/> </string>
      <uInt32 id=\"34\" name=\"MsgSeqNum\"/>
      <uInt64 id=\"52\" name=\"SendingTime\"/>
      <string id=\"43\" name=\"PosDupFlag\" presence=\"optional\"> <default/> </string>
      <uInt32 id=\"75\" name=\"TradeDate\"/>
      <sequence name=\"MDEntries\">
         <length id=\"268\" name=\"NoMDEntries\"/>
         <uInt32 id=\"279\" name=\"MDUpdateAction\"> <copy value=\"1\"/> </uInt32>
         <uInt32 id=\"1023\" name=\"MDPriceLevel\"> <increment value=\"1\"/> </uInt32>
         <string id=\"269\" name=\"MDEntryType\"> <copy value=\"0\"/> </string>
         <uInt32 id=\"273\" name=\"MDEntryTime\"> <copy/> </uInt32>
         <uInt32 id=\"22\" name=\"SecurityIDSource\"> <constant value=\"8\"/> </uInt32>
         <uInt32 id=\"48\" name=\"SecurityID\"> <copy/> </uInt32>
         <uInt32 id=\"83\" name=\"RptSeq\"> <increment/> </uInt32>
         <decimal id=\"270\" name=\"MDEntryPx\"> <exponent>
               <default value=\"0\"/>
            </exponent>
            <mantissa>
               <delta/>
            </mantissa>
         </decimal>
         <int32 id=\"271\" name=\"MDEntrySize\"> <delta/> </int32>
         <uInt32 id=\"346\" name=\"NumberOfOrders\"> <delta/> </uInt32>
         <string id=\"336\" name=\"TradingSessionID\"> <default value=\"2\"/> </string>
      </sequence>
   </template>
   </templates>").

-define(xmlDescr1,
   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
    <templates xmlns=\"http://www.fixprotocol.org/ns/fast/td/1.1\">
	   <template name=\"d\" id=\"2115\" xmlns=\"http://www.fixprotocol.org/ns/fast/td/1.1\">
         <string name=\"MessageType\" id=\"35\"><constant value=\"d\" /></string>
         <string name=\"ApplVerID\" id=\"1128\"><constant value=\"9\"/></string>
         <string name=\"BeginString\" id=\"8\"><constant value=\"FIXT.1.1\"/></string>
         <string name=\"SenderCompID\" id=\"49\"><constant value=\"MOEX\"/></string>
         <uInt32 name=\"MsgSeqNum\" id=\"34\"><increment/></uInt32>
         <uInt64 name=\"SendingTime\" id=\"52\"></uInt64>
         <string name=\"MessageEncoding\" id=\"347\"><default value=\"UTF-8\"/></string>
         <int32 name=\"TotNumReports\" id=\"911\" presence=\"optional\"></int32>
         <string name=\"Symbol\" id=\"55\" presence=\"optional\"></string>
         <byteVector name=\"SecurityID\" id=\"48\" presence=\"optional\"></byteVector>
         <byteVector name=\"SecurityIDSource\" id=\"22\" presence=\"optional\"></byteVector>
         <int32 name=\"Product\" id=\"460\" presence=\"optional\"></int32>
         <byteVector name=\"CFICode\" id=\"461\" presence=\"optional\"></byteVector>
         <byteVector name=\"SecurityType\" id=\"167\" presence=\"optional\"></byteVector>
         <uInt32 name=\"MaturityDate\" id=\"541\" presence=\"optional\"></uInt32>
         <uInt32 name=\"SettlDate\" id=\"64\" presence=\"optional\"></uInt32>
         <string name=\"SettleType\" id=\"5459\" presence=\"optional\"></string>
         <decimal name=\"OrigIssueAmt\" id=\"5850\" presence=\"optional\"></decimal>
         <uInt32 name=\"CouponPaymentDate\" id=\"224\" presence=\"optional\"></uInt32>
         <decimal name=\"CouponRate\" id=\"223\" presence=\"optional\"></decimal>
         <uInt32 name=\"SettlFixingDate\" id=\"9119\" presence=\"optional\"></uInt32>
         <decimal name=\"DividendNetPx\" id=\"9982\" presence=\"optional\"></decimal>
         <byteVector name=\"SecurityDesc\" id=\"107\" presence=\"optional\"></byteVector>
         <byteVector name=\"EncodedSecurityDesc\" id=\"351\" presence=\"optional\"></byteVector>
         <byteVector name=\"QuoteText\" id=\"9696\" presence=\"optional\"></byteVector>
         <sequence name=\"GroupInstrAttrib\" presence=\"optional\">
            <length name=\"NoInstrAttrib\" id=\"870\"/>
            <int32 name=\"InstrAttribType\" id=\"871\"></int32>
            <byteVector name=\"InstrAttribValue\" id=\"872\" presence=\"optional\"></byteVector>
         </sequence>
         <string name=\"Currency\" id=\"15\" presence=\"optional\"></string>
         <sequence name=\"MarketSegmentGrp\" presence=\"optional\">
            <length name=\"NoMarketSegments\" id=\"1310\"/>
            <decimal name=\"RoundLot\" id=\"561\" presence=\"optional\"></decimal>
            <sequence name=\"TradingSessionRulesGrp\" presence=\"optional\">
               <length name=\"NoTradingSessionRules\" id=\"1309\"/>
               <string name=\"TradingSessionID\" id=\"336\"></string>
               <string name=\"TradingSessionSubID\" id=\"625\" presence=\"optional\"></string>
               <int32 name=\"SecurityTradingStatus\" id=\"326\" presence=\"optional\"></int32>
               <int32 name=\"OrderNote\" id=\"9680\" presence=\"optional\"></int32>
               </sequence>
         </sequence>
         <string name=\"SettlCurrency\" id=\"120\" presence=\"optional\"></string>
         <int32 name=\"PriceType\" id=\"423\" presence=\"optional\"></int32>
         <string name=\"StateSecurityID\" id=\"5217\" presence=\"optional\"></string>
         <byteVector name=\"EncodedShortSecurityDesc\" id=\"5383\" presence=\"optional\"></byteVector>
         <byteVector name=\"MarketCode\" id=\"5385\" presence=\"optional\"></byteVector>
         <decimal name=\"MinPriceIncrement\" id=\"969\" presence=\"optional\"></decimal>
         <decimal name=\"MktShareLimit\" id=\"5387\" presence=\"optional\"></decimal>
         <decimal name=\"MktShareThreshold\" id=\"5388\" presence=\"optional\"></decimal>
         <decimal name=\"MaxOrdersVolume\" id=\"5389\" presence=\"optional\"></decimal>
         <decimal name=\"PriceMvmLimit\" id=\"5470\" presence=\"optional\"></decimal>
         <decimal name=\"FaceValue\" id=\"5508\" presence=\"optional\"></decimal>
         <decimal name=\"BaseSwapPx\" id=\"5556\" presence=\"optional\"></decimal>
         <decimal name=\"RepoToPx\" id=\"5677\" presence=\"optional\"></decimal>
         <decimal name=\"BuyBackPx\" id=\"5558\" presence=\"optional\"></decimal>
         <uInt32 name=\"BuyBackDate\" id=\"5559\" presence=\"optional\"></uInt32>
         <decimal name=\"NoSharesIssued\" id=\"7595\" presence=\"optional\"></decimal>
         <decimal name=\"HighLimit\" id=\"9199\" presence=\"optional\"></decimal>
         <decimal name=\"LowLimit\" id=\"9200\" presence=\"optional\"></decimal>
         <int32 name=\"NumOfDaysToMaturity\" id=\"10508\" presence=\"optional\"></int32>
	   </template>
   </templates>").

-define(msg,
       [{<<"ApplVerID">>,<<"13">>},
        {<<"MessageType">>,<<"X">>},
        {<<"SenderCompID">>,<<"XXXXX">>},
        {<<"MsgSeqNum">>,3033666},
        {<<"SendingTime">>,20100713060000051},
        {<<"TradeDate">>,20100713},
        {<<"MDEntries">>,
         [[{<<"MDUpdateAction">>,1},
           {<<"MDPriceLevel">>,2},
           {<<"MDEntryType">>,<<"0">>},
           {<<"MDEntryTime">>,60000000},
           {<<"SecurityIDSource">>,8},
           {<<"SecurityID">>,26714},
           {<<"RptSeq">>,5049472},
           {<<"MDEntryPx">>,{107425,0}},
           {<<"MDEntrySize">>,189},
           {<<"NumberOfOrders">>,30},
           {<<"TradingSessionID">>,<<"2">>}],
          [{<<"MDUpdateAction">>,1},
           {<<"MDPriceLevel">>,3},
           {<<"MDEntryType">>,<<"0">>},
           {<<"MDEntryTime">>,60000000},
           {<<"SecurityIDSource">>,8},
           {<<"SecurityID">>,26714},
           {<<"RptSeq">>,5049473},
           {<<"MDEntryPx">>,{1074,2}},
           {<<"MDEntrySize">>,246},
           {<<"NumberOfOrders">>,34},
           {<<"TradingSessionID">>,<<"2">>}],
          [{<<"MDUpdateAction">>,1},
           {<<"MDPriceLevel">>,1},
           {<<"MDEntryType">>,<<"1">>},
           {<<"MDEntryTime">>,60000000},
           {<<"SecurityIDSource">>,8},
           {<<"SecurityID">>,26714},
           {<<"RptSeq">>,5049474},
           {<<"MDEntryPx">>,{107475,0}},
           {<<"MDEntrySize">>,72},
           {<<"NumberOfOrders">>,18},
           {<<"TradingSessionID">>,<<"2">>}]]}]).

-define(msg1,
       [{<<"ApplVerID">>,<<"13">>},
        {<<"MessageType">>,<<"X">>},
        {<<"SenderCompID">>,<<"XXXXX">>},
        {<<"MsgSeqNum">>,3033667},
        {<<"SendingTime">>,20100713060000052},
        {<<"TradeDate">>,20100713},
        {<<"MDEntries">>,
         [[{<<"MDUpdateAction">>,1},
           {<<"MDPriceLevel">>,1},
           {<<"MDEntryType">>,<<"0">>},
           {<<"MDEntryTime">>,60000000},
           {<<"SecurityIDSource">>,8},
           {<<"SecurityID">>,26714},
           {<<"RptSeq">>,5049475},
           {<<"MDEntryPx">>,{10745,1}},
           {<<"MDEntrySize">>,95},
           {<<"NumberOfOrders">>,12},
           {<<"TradingSessionID">>,<<"2">>}]]}]).

-define(data,<<192,129,1,57,20,194,35,90,47,95,45,49,66,179,9,74,108,233,131,174,130,28,78,14,128,
               1,80,218,2,52,25,128,6,71,161,1,189,158,129,130,121,65,145,185,132,176,129,177,6,63,161,
               126,210,240,128,1,57,20,195,35,90,47,95,45,49,66,180,9,74,108,233,129,177,129,176,129,122,
               12,166,151,250,192,212,1,57,20,196,35,90,47,95,45,49,66,195,9,74,108,233,129,30,176,1,80,
               218,2,52,25,132,129,0,83,249,134,131,254,3,46,144,28,78,14,128,131,192,237,1,57,20,197,35,
               90,47,95,45,49,66,195,9,74,108,233,130,45,224,130,1,80,218,2,52,25,133,129,0,83,249,141,
               28,78,14,128,0,219,128,17,192,177,130,127,52,186,158,0,198,128,192,211,1,57,20,198,35,90,
               47,95,45,49,66,200,9,74,108,233,130,147,177,2,52,25,135,130,127,52,186,190,144,128,6,63,
               210,134,136,128,1,57,20,199,35,90,47,95,45,49,66,200,9,74,108,233,130,160,131,128,254,255,
               129>>).

-define(data1, <<224,16,195,58,236,2,46,66,1,37,102,108,61,215,0,104,193,82,85,48,48,48,65,48,74,84,77,71,183,
                 141,82,85,48,48,48,65,48,74,84,77,71,55,130,52,132,135,68,66,88,88,88,88,133,67,79,82,80,9,82,
                 96,187,9,79,12,190,128,136,129,9,79,16,187,251,33,142,128,128,145,82,117,115,71,105,100,114,
                 111,40,80,65,79,41,32,48,55,157,208,160,209,131,209,129,208,147,208,184,208,180,209,128,
                 208,190,32,40,208,159,208,144,208,158,41,32,48,55,128,131,155,130,52,136,132,49,56,50,82,85,
                 194,130,129,129,130,80,83,82,196,78,193,147,130,85,83,196,130,52,45,48,55,45,53,53,48,51,
                 56,45,197,147,208,160,209,131,209,129,208,147,208,184,208,180,209,128,208,190,48,55,133,82,
                 69,80,84,252,129,128,128,128,128,132,129,128,128,131,129,9,81,68,160,136,129,128,128,128>>).

decode_test() ->
   {ok, Context} = erlang_fast:create_context(?xmlDescr, [], fun logger/2),
   {ok, {_, Msg, Data1, Context1}} = erlang_fast:decode(?data, Context),
   ?assertEqual(?msg, Msg),
   {ok, {_, Msg1, _, _}} = erlang_fast:decode(Data1, Context1),
   ?assertEqual(?msg1, Msg1).

decode1_test() ->
   {ok, Context} = erlang_fast:create_context(?xmlDescr1, [], fun logger/2),
   {ok, {_, Msg, Data1, Context1}} = erlang_fast:decode(?data1, Context).

encode_test() ->
   {ok, Context} = erlang_fast:create_context(?xmlDescr, [], fun logger/2),
   {ok, {Data, Context1}} = erlang_fast:encode(1, ?msg, Context),
   ?assertEqual(<<192,129,1,57,20,194,35,90,47,95,45,49,66,179,9,74,108,233,131,174,130,28,78,14,128,1,80,218,2,52,25,
                  128,6,71,161,1,189,158,129,130,121,65,145,185,132,176,129,177,6,63,161,
   126, 210, 240>>, Data),
   {ok, {Data1, _}} = erlang_fast:encode(1, ?msg1, Context1),
   ?assertEqual(<<128,1,57,20,195,35,90,47,95,45,49,66,180,9,74,108,233,129,177,129,176,129,122,12,166,151,250>>, Data1).

find_template_test() ->
   {ok, Context} = erlang_fast:create_context(?xmlDescr, [], fun logger/2),
   ?assertMatch({template, <<"MDIncRefresh">>, _, 1, _, "1", _}, erlang_fast_templates:get_by_id(1,
         Context#context.templates#templates.tlist)).

logger([], _) ->
  ok;
logger(Err, Value) ->
  io:format("~p: ~p~n", [Err, Value]).

