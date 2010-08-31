-module(erlang_fast_decode_uint).


-export([decode_instruction/3]).

-include("erlang_fast_template.hrl").
-include("erlang_fast_context.hrl").
-include("erlang_fast_common.hrl").

-import(erlang_fast_utils,
   [
      is_nullable/1
      ,apply_delta/3
   ]).

-import(erlang_fast_decode_types,
   [
      decode_uint/2
   ]).

decode_instruction(Data, Instr, Context) ->
   {uint, Context, Data}.
