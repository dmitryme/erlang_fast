-module(erlang_fast_decode_types).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-export([
      decode_pmap/1
      ,decode_type/3
      ,decode_delta/3
   ]).

%% ====================================================================================================================
%% publics
%% ====================================================================================================================

decode_type(Type, Data, Nullable) when (Type == int32) or (Type == int64)->
   decode_int(Data, Nullable);

decode_type(Type, Data, Nullable) when (Type == uInt32) or (Type == uInt64)->
   decode_uint(Data, Nullable);

decode_type(string, Data, Nullable) ->
   decode_string(Data, Nullable);

decode_type(unicode, Data, Nullable) ->
   decode_vector(Data, Nullable);

decode_type(byteVector, Data, Nullable) ->
   decode_vector(Data, Nullable);

decode_type(decimal, Data, Nullable) ->
   decode_scaled(Data, Nullable).

decode_delta(Type, Data, Nullable)
   when (Type == int32) or (Type == int64) or (Type == uInt32) or (Type == uInt64) ->
      decode_int(Data, Nullable);

decode_delta(Type, Data, Nullable) when (Type == string) ->
   decode_string_delta(Data, Nullable);

decode_delta(Type, Data, Nullable)
   when (Type == unicode) or (Type == byteVector) ->
      decode_vector_delta(Data, Nullable);

decode_delta(Type, Data, Nullable) when (Type == decimal) ->
   decode_scaled(Data, Nullable).

%% ====================================================================================================================
%% privates
%% ====================================================================================================================

decode_int(<<1:1, 0:7/integer, Rest/binary>>, true) ->
   {null, [], Rest};
decode_int(Data, Nullable) ->
   case decode_int_aux(Data) of
      R = {Value, _Err, _Rest} when Nullable == true andalso Value < 0 ->
         R;
      {Value, Err, Rest} when Nullable == true andalso Value >=0 ->
         {Value - 1, Err, Rest};
      Result when Nullable == false ->
         Result
   end.

decode_uint(<<1:1, 0:7/integer, Rest/binary>>, true) ->
   {null, [], Rest};
decode_uint(Data, Nullable) ->
   Result = decode_uint_aux(Data),
   case Result of
      {Value, Err, Rest} when Nullable == true ->
         {Value - 1, Err, Rest};
      _ ->
         Result
   end.

decode_string(<<1:1, 0:7/integer, Rest/binary>>, false) ->
   {<<"">>, [], Rest};
decode_string(<<0:8/integer, 1:1, 0:7/integer, Rest/binary>>, false) ->
   {<<0>>, [], Rest};
decode_string(<<1:1, 0:7/integer, Rest/binary>>, true) ->
   {null, [], Rest};
decode_string(<<0:8/integer, 1:1, 0:7/integer, Rest/binary>>, true) ->
   {<<"">>, [], Rest};
decode_string(<<0:8/integer, 0:8/integer, 1:1, 0:7/integer, Rest/binary>>, true) ->
   {<<0>>, [], Rest};
decode_string(Data, _Nullable) ->
   case decode_string_aux(Data, <<>>) of
      {<<0, Next, Remainder/binary>>, Rest} when Next > 0 ->
         {<<Next, Remainder>>, ['ERR R9'], Rest};
      {<<0, 0, Remainder/binary>>, Rest} ->
         {<<Remainder>>, ['ERR R9'], Rest};
      {Str, Rest} ->
         {Str, [], Rest}
   end.

decode_string_delta(Data, Nullable) ->
   case decode_int(Data, Nullable) of
      {null, Err, Rest} ->
         {null, Err, Rest};
      {Len, Err, Rest} ->
         {String, Err1, Rest2} = decode_string(Rest, Nullable),
         {{Len, String}, Err ++ Err1, Rest2}
   end.

decode_vector(Data, Nullable) ->
   Size = decode_uint(Data, Nullable),
   case Size of
      {null, Err, Rest} ->
         {null, Err, Rest};
      {Value, Err, Rest} ->
         case erlang:byte_size(Rest) >= Value of
            true ->
               {Vector, Rest2} = erlang:split_binary(Rest, Value),
               {Vector, Err, Rest2};
            false ->
               throw(not_enough_data)
         end
   end.

decode_vector_delta(Data, Nullable) ->
   case decode_int(Data, Nullable) of
      {null, Err, Rest} ->
         {null, Err, Rest};
      {Len, Err, Rest} ->
         case decode_vector(Rest, Nullable) of
            {Vector, Err1, Rest2} ->
               {{Len, Vector}, Err ++ Err1, Rest2}
         end
   end.

decode_scaled(Data, Nullable) ->
   Exponent = decode_int(Data, Nullable),
   case Exponent of
      Res = {null, _Err, _Rest} ->
         Res;
      {ExpValue, Err, Rest} ->
         {MantValue, Err1, Rest1} = decode_int(Rest, false),
         {{MantValue, ExpValue}, lists:flatten([Err, Err1]), Rest1}
   end.

decode_pmap(Data) ->
   case decode_pmap_aux(Data) of
      {PMap, Rest} when erlang:bit_size(PMap) > 7 ->
         case PMap of
            <<_P:7, 0:1>> ->
               {PMap, ['ERR R7'], Rest};
            _ ->
               {PMap, [], Rest}
         end;
      {PMap, Rest} ->
         {PMap, [], Rest}
   end.

%% ====================================================================================================================
%% decoding details
%% ====================================================================================================================

%% decode signed integer

decode_int_aux(<<>>) ->
   throw(not_enough_data);
decode_int_aux(<<1:1, 1:1, Data:6/integer, Rest/binary>>) ->
   {(-1 bsl 6) bor Data, [], Rest};
decode_int_aux(<<1:1, Data:7/integer, Rest/binary>>) ->
   {Data, [], Rest};
decode_int_aux(<<0:1, 1:1, Data:6/integer, Rest/binary>>) ->
   decode_int_aux(Rest, (-1 bsl 6) bor Data, []);
decode_int_aux(<<0:1, Data:7/integer, Rest/binary>>) ->
   decode_int_aux(Rest, Data, []).

decode_int_aux(<<>>, _Acc, _Err) ->
   throw(not_enough_data);
decode_int_aux(<<1:1, Data:7/integer, Rest/binary>>, Acc, Err) ->
   {(Acc bsl 7) bor Data, Err, Rest};
decode_int_aux(<<0:1, Data:7/integer, Rest/binary>>, Acc, Err) ->
   NewVal = (Acc bsl 7) bor Data,
   case NewVal =:= Acc of
      true ->
         decode_int_aux(Rest, NewVal, ['ERR R6'|Err]);
      false ->
         decode_int_aux(Rest, NewVal, Err)
   end.

%% decode unsigned integer

decode_uint_aux(<<>>) ->
   throw(not_enough_data);
decode_uint_aux(<<1:1, Data:7/integer, Rest/binary>>) ->
   {Data, [], Rest};
decode_uint_aux(<<0:1, Data:7/integer, Rest/binary>>) ->
   decode_uint_aux(Rest, Data, []).

decode_uint_aux(<<>>, _Acc, _Err) ->
   throw(not_enough_data);
decode_uint_aux(<<1:1, Data:7/integer, Rest/binary>>, Acc, Err) ->
   {(Acc bsl 7) bor Data, Err, Rest};
decode_uint_aux(<<0:1, Data:7/integer, Rest/binary>>, Acc, Err) ->
   NewVal = (Acc bsl 7) bor Data,
   case NewVal =:= Acc of
      true ->
         decode_uint_aux(Rest, NewVal, ['ERR R6'|Err]);
      false ->
         decode_uint_aux(Rest, NewVal, Err)
   end.

%% decode ASCII string

decode_string_aux(<<>>, _Acc) ->
   throw(not_enough_data);
decode_string_aux(<<0:1, Ch:7/integer, Rest/binary>>, Acc) ->
   decode_string_aux(Rest, <<Acc/binary, Ch>>);
decode_string_aux(<<1:1, Ch:7/integer, Rest/binary>>, Acc) ->
   {<<Acc/binary, Ch>>, Rest}.

%% decode pmap
decode_pmap_aux(<<>>) ->
   throw(not_enough_data);
decode_pmap_aux(<<0:1, Data:7/bitstring, Rest/binary>>) ->
   decode_pmap_aux(Rest, Data);
decode_pmap_aux(<<1:1, Data:7/bitstring, Rest/binary>>) ->
   {Data, Rest}.

decode_pmap_aux(<<>>, _Acc) ->
   throw(not_enough_data);
decode_pmap_aux(<<0:1, Data:7/bitstring, Rest/binary>>, Acc) ->
   decode_pmap_aux(Rest, <<Acc/bitstring, Data/bitstring>>);
decode_pmap_aux(<<1:1, Data:7/bitstring, Rest/binary>>, Acc) ->
   {<<Acc/bitstring, Data/bitstring>>, Rest}.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

decode_int_test() ->
  ?assertEqual({942755, [], <<>>}, decode_int(<<16#39, 16#45, 16#a4>>, true)),
  ?assertEqual({942755, [], <<>>}, decode_int(<<16#39, 16#45, 16#a3>>, false)),
  ?assertEqual({-942755, [], <<>>}, decode_int(<<16#46, 16#3a, 16#dd>>, true)),
  ?assertEqual({-7942755, [], <<>>}, decode_int(<<16#7c, 16#1b, 16#1b, 16#9d>>, false)),
  ?assertEqual({8193, [], <<>>}, decode_int(<<16#00, 16#40, 16#81>>, false)),
  ?assertEqual({-8193, [], <<>>}, decode_int(<<16#7f, 16#3f, 16#ff>>, false)),
  ?assertEqual({null, [], <<>>}, decode_int(<<16#80>>, true)),
  ?assertEqual({11, [], <<>>}, decode_int(<<2#10001011>>, false)),
  ?assertEqual({-2, [], <<>>}, decode_int(<<16#fe>>, true)),
  ?assertEqual({64, [], <<>>}, decode_int(<<16#00, 16#c0>>, false)),
  ?assertThrow(not_enough_data, decode_int(<<2#0001>>, true)),
  ?assertThrow(not_enough_data, decode_int(<<2#00011111>>, true)),
  ?assertThrow(not_enough_data, decode_int(<<>>, true)).

decode_uint_test() ->
  ?assertEqual({null, [], <<>>}, decode_uint(<<16#80>>, true)),
  ?assertEqual({0, [], <<>>}, decode_uint(<<16#81>>, true)),
  ?assertEqual({1, [], <<>>}, decode_uint(<<16#82>>, true)),
  ?assertEqual({0, [], <<>>}, decode_uint(<<16#80>>, false)),
  ?assertEqual({1, [], <<>>}, decode_uint(<<16#81>>, false)),
  ?assertEqual({942755, [], <<>>}, decode_uint(<<16#39, 16#45, 16#a3>>, false)),
  ?assertThrow(not_enough_data, decode_uint(<<2#0001>>, true)),
  ?assertThrow(not_enough_data, decode_uint(<<2#00011111>>, true)),
  ?assertThrow(not_enough_data, decode_uint(<<>>, true)).

decode_string_test() ->
   ?assertEqual({<<"ABC">>, [], <<>>}, decode_string(<<16#41, 16#42, 16#c3>>, true)),
   ?assertEqual({null, [], <<>>}, decode_string(<<16#80>>, true)),
  ?assertEqual({<<>>, [], <<>>}, decode_string(<<16#00, 16#80>>, true)),
  ?assertEqual({<<"ABC">>, [], <<>>}, decode_string(<<16#41, 16#42, 16#c3>>, false)),
  ?assertEqual({<<>>, [], <<>>}, decode_string(<<16#80>>, false)),
  ?assertThrow(not_enough_data, decode_string(<<>>, false)),
  ?assertThrow(not_enough_data, decode_string(<<16#41, 16#42, 16#43>>, false)).

decode_vector_test() ->
  ?assertEqual({null, [], <<>>}, decode_vector(<<16#80>>, true)),
  ?assertEqual({<<16#41, 16#42, 16#43>>, [], <<>>}, decode_vector(<<16#84, 16#41, 16#42, 16#43>>, true)),
  ?assertEqual({<<>>, [], <<>>}, decode_vector(<<16#81>>, true)),
  ?assertEqual({<<>>, [], <<>>}, decode_vector(<<16#80>>, false)),
  ?assertEqual({<<16#41, 16#42, 16#43>>, [], <<>>}, decode_vector(<<16#83, 16#41, 16#42, 16#43>>, false)),
  ?assertThrow(not_enough_data, decode_vector(<<16#83, 16#41, 16#42>>, false)),
  ?assertThrow(not_enough_data, decode_vector(<<16#83>>, false)),
  ?assertThrow(not_enough_data, decode_vector(<<>>, false)).

decode_decimal_test() ->
  ?assertEqual({{942755, 2}, [], <<>>}, decode_scaled(<<16#82, 16#39, 16#45, 16#a3>>, false)),
  ?assertEqual({{9427550, 1}, [], <<>>}, decode_scaled(<<16#81, 16#04, 16#3f, 16#34, 16#de>>, false)),
  ?assertEqual({{942755,2}, [], <<>>}, decode_scaled(<<16#83, 16#39, 16#45, 16#a3>>, true)),
  ?assertEqual({{942755,-2}, [], <<>>}, decode_scaled(<<16#fe, 16#39, 16#45, 16#a3>>, false)),
  ?assertEqual({{-942755,-2}, [], <<>>}, decode_scaled(<<16#fe, 16#46, 16#3a, 16#dd>>, true)),
  ?assertEqual({{-8193,-3}, [], <<>>}, decode_scaled(<<16#fd, 16#7f, 16#3f, 16#ff>>, true)).

decode_pmap_test() ->
   ?assertEqual({<<36:7>>, [], <<>>}, decode_pmap(<<16#a4>>)),
   ?assertThrow(not_enough_data, decode_pmap(<<2#00001111>>)),
   ?assertEqual({<<115, 21, 4:5>>, [], <<>>}, decode_pmap(<<16#39, 16#45, 16#a4>>)),
   ?assertThrow(not_enough_data, decode_pmap(<<16#39, 16#45, 16#46>>)).

-endif.
