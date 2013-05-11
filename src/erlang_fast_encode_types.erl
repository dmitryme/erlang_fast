-module(erlang_fast_encode_types).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
      encode_pmap/1
      ,encode_type/3
      ,encode_delta/3
      ]).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================================================================
%% publics
%% ====================================================================================================================

encode_type(Type, Value, Nullable) when (Type == int32) or (Type == int64) ->
   encode_int(Value, Nullable);

encode_type(Type, Value, Nullable) when (Type == uInt32) or (Type == uInt64) ->
   encode_uint(Value, Nullable);

encode_type(string, Value, Nullable) when is_list(Value) ->
   encode_type(string, list_to_binary(Value), Nullable);

encode_type(string, Value, Nullable) ->
   encode_string(Value, Nullable);

encode_type(Type, Value, Nullable) when (Type == unicode) or (Type == byteVector) ->
   encode_vector(Value, Nullable);

encode_type(decimal, null, Nullable) ->
   encode_uint(null, Nullable);

encode_type(decimal, Value, Nullable) when is_float(Value) ->
   encode_type(decimal, erlang_fast_utils:float_to_decimal(Value), Nullable);

encode_type(decimal, {Mantissa, Exponent}, Nullable) ->
   <<(encode_int(Exponent, Nullable))/bits, (encode_int(Mantissa, false))/bits>>.

encode_delta(Type, null, Nullable) when (Type == string) or (Type == unicode) or (Type == byteVector) ->
   encode_int(null, Nullable);

encode_delta(decimal, null, Nullable) ->
   encode_int(null, Nullable);

encode_delta(Type, Value, Nullable) when (Type == int32) or (Type == int64) or (Type == uInt32) or (Type == uInt64) ->
   encode_int(Value, Nullable);

encode_delta(string, {delta, Len, Value}, Nullable) ->
   <<(encode_int(Len, Nullable))/bits, (encode_string(Value, Nullable))/bits>>;

encode_delta(Type, {delta, Len, Value}, Nullable) when (Type == unicode) or (Type == byteVector) ->
   <<(encode_int(Len, Nullable))/bits, (encode_vector(Value, Nullable))/bits>>;

encode_delta(decimal, {MDelta, EDelta}, Nullable) ->
   <<(encode_int(EDelta, Nullable))/bits, (encode_int(MDelta, false))/bits>>.

%% ====================================================================================================================
%% privates
%% ====================================================================================================================

%% encode int

encode_uint(null, _) ->
   <<2#10000000:8>>;
encode_uint(Value, false) ->
   encode_number_aux(Value);
encode_uint(Value, true) ->
   encode_number_aux(Value + 1).

%% encode int

encode_int(null, _) ->
   <<2#10000000:8>>;
encode_int(Value, Nullable) ->
   Res = <<_:1, SignBit:1, _/bits>> =
   case (Value >= 0) of
      true when Nullable == true ->
         encode_number_aux(Value + 1);
      _ ->
         encode_number_aux(Value)
   end,
   case Value >= 0 of
     true when SignBit == 1 ->
        <<0:8, Res/bytes>>;
     false when SignBit == 0 ->
        <<16#7f:8, Res/bytes>>;
      _ ->
         Res
   end.

%% encode ASCII string

encode_string(null, _Nullable) ->
   <<16#80>>;
encode_string(Str, Nullable) ->
   case encode_string_aux(Str) of
      <<>> when Nullable == false ->
         <<16#80>>;
      <<16#80>> when Nullable == false ->
         <<0:8, 16#80>>;
      <<>> when Nullable == true ->
         <<0:8, 16#80>>;
      <<16#80>> when Nullable == true ->
         <<0:8, 0:8, 16#80>>;
      Res ->
         Res
   end.

%% encode byte vector

encode_vector(null, true) ->
   encode_uint(null, true);
encode_vector(Binary, Nullable) ->
   <<(encode_uint(byte_size(Binary), Nullable))/bytes, Binary/bytes>>.

%% encode pmap

encode_pmap(PMap) ->
   encode_pmap_aux(trim_zero_tail(PMap)).

%% ====================================================================================================================
%% encoding details
%% ====================================================================================================================

%% encode number

encode_number_aux(Val) ->
   encode_number_aux(Val, <<1:1>>).

encode_number_aux(0, <<1:1>>) ->
   <<2#10000000:8>>;
encode_number_aux(0, _) ->
   <<>>;
encode_number_aux(-1, <<1:1>>) ->
   <<2#11111111:8>>;
encode_number_aux(-1, _) ->
   <<>>;
encode_number_aux(Value, StopBit) ->
   <<(encode_number_aux(Value bsr 7, <<0:1>>))/bits, StopBit/bits, Value:7>>.

%% encode ASCII string

encode_string_aux(<<>>) ->
   <<>>;
encode_string_aux(<<_:1, Chr:7/bits>>) ->
   <<1:1, Chr/bits>>;
encode_string_aux(<<_:1, Chr:7/bits, Rest/bits>>) ->
   <<0:1, Chr/bits, (encode_string_aux(Rest))/bits>>.

%% encode pmap

encode_pmap_aux(<<>>) ->
   <<>>;
encode_pmap_aux(Data) when bit_size(Data) =< 7 ->
  TailSize = 7 - bit_size(Data),
  <<1:1, Data/bits, 0:TailSize>>;
encode_pmap_aux(<<Data:7/bits, Rest/bits>>) ->
  <<0:1, Data/bits, (encode_pmap_aux(Rest))/bits>>.

trim_zero_tail(<<>>) ->
   <<>>;
trim_zero_tail(Bin) when bit_size(Bin) =< 7 ->
   Bin;
trim_zero_tail(Bin) ->
   HeadSize = bit_size(Bin) - 1,
   <<HeadBin:HeadSize/bits, LastBit:1>> = Bin,
   case LastBit of
      1 ->
         Bin;
      0 ->
         trim_zero_tail(HeadBin)
   end.

%% ====================================================================================================================
%% unit testing
%% ====================================================================================================================

-ifdef(TEST).

decode_int_test() ->
   ?assertEqual(<<255>>, encode_int(-1, true)),
   ?assertEqual(<<255>>, encode_int(-1, false)),
   ?assertEqual(<<16#39, 16#45, 16#a4>>, encode_int(942755, true)),
   ?assertEqual(<<16#39, 16#45, 16#a3>>, encode_int(942755, false)),
   ?assertEqual(<<16#46, 16#3a, 16#dd>>, encode_int(-942755, true)),
   ?assertEqual(<<16#7c, 16#1b, 16#1b, 16#9d>>, encode_int(-7942755, false)),
   ?assertEqual(<<16#00, 16#40, 16#81>>, encode_int(8193, false)),
   ?assertEqual(<<16#7f, 16#3f, 16#ff>>, encode_int(-8193, false)),
   ?assertEqual(<<16#80>>, encode_int(null, true)),
   ?assertEqual(<<2#10001011>>, encode_int(11, false)),
   ?assertEqual(<<16#fe>>, encode_int(-2, true)),
   ?assertEqual(<<16#0, 16#ed>>, encode_int(109, false)).

decode_uint_test() ->
   ?assertEqual(<<16#39, 16#45, 16#a4>>, encode_uint(942755, true)),
   ?assertEqual(<<16#39, 16#45, 16#a3>>, encode_uint(942755, false)),
   ?assertEqual(<<16#80>>, encode_uint(null, true)),
   ?assertEqual(<<16#81>>, encode_uint(0, true)),
   ?assertEqual(<<16#82>>, encode_uint(1, true)),
   ?assertEqual(<<16#80>>, encode_uint(0, false)),
   ?assertEqual(<<16#81>>, encode_uint(1, false)).

decode_string_test() ->
   ?assertEqual(<<16#41, 16#42, 16#c3>>, encode_string(<<"ABC">>, true)),
   ?assertEqual(<<16#80>>, encode_string(null, true)),
   ?assertEqual(<<16#00, 16#80>>, encode_string(<<"">>, true)),
   ?assertEqual(<<16#41, 16#42, 16#c3>>, encode_string(<<"ABC">>, false)),
   ?assertEqual(<<16#80>>, encode_string(<<"">>, false)).

decode_vector_test() ->
   ?assertEqual(<<16#80>>, encode_vector(null, true)),
   ?assertEqual(<<16#84, 16#41, 16#42, 16#43>>, encode_vector(<<16#41, 16#42, 16#43>>, true)),
   ?assertEqual(<<16#81>>, encode_vector(<<>>, true)),
   ?assertEqual(<<16#80>>, encode_vector(<<>>, false)),
   ?assertEqual(<<16#83, 16#41, 16#42, 16#43>>, encode_vector(<<16#41, 16#42, 16#43>>, false)).

decode_pmap_test() ->
   ?assertEqual(<<16#a4>>, encode_pmap(<<36:7>>)),
   ?assertEqual(<<16#39, 16#45, 16#a4>>, encode_pmap(<<115, 21, 4:5>>)).

-endif.
