-define(msg1, {1,
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
           {<<"TradingSessionID">>,<<"2">>}]]}]}).

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
   {Msg, Data1, Context1} = erlang_fast:decode(?data, Context),
   ?assertEqual(?msg, Msg),
   {Msg1, _, _} = erlang_fast:decode(Data1, Context1),
   ?assertEqual(?msg1, Msg1).

encode_test() ->
   Context = erlang_fast:create_context(?xmlDescr, fun logger/2),
   {Data, Context1} = erlang_fast:encode(?msg, Context),
   ?assertEqual(<<192, 129, 1, 57, 20, 194, 35, 90, 47, 95, 45, 49, 66, 179, 9, 74, 108, 233, 131, 174, 130, 28, 78, 14, 128,
   1, 80, 218, 2, 52, 25, 128, 6, 71, 161, 1, 189, 158, 129, 130, 121, 65, 145, 185, 132, 176, 129, 177, 6, 63, 161,
   126, 210, 240>>, Data),
   {Data1, _} = erlang_fast:encode(?msg1, Context1),
   ?assertEqual(<<128, 1, 57, 20, 195, 35, 90, 47, 95, 45, 49, 66, 180, 9, 74, 108, 233, 129, 177, 129, 176, 129, 122,
   12, 166, 151, 250>>, Data1).

find_template_test() ->
   Context = erlang_fast:create_context(?xmlDescr, fun logger/2),
   ?assertMatch({template, <<"MDIncRefresh">>, _, 1, _, "1", _}, erlang_fast_templates:get_by_id(1,
         Context#context.templates#templates.tlist)).

logger([], _) ->
   ok;
logger(Err, Value) ->
   io:format("~p: ~p~n", [Err, Value]).