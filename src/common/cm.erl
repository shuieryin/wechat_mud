%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Common API module. This module provides APIs that handles generic handlings.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(cm).
-author("Shuieryin").

%% API
-export([
    q/0,
    observer/0,
    general_target/2,
    execute_command/2,
    execute_sync_command/2,
    pkcs7pad/1,
    pkcs7unpad/1
]).

-define(OBSERVER_NODE, 'macbook@192.168.1.110').

-include_lib("eunit/include/eunit.hrl").
-include("../data_type/scene_info.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Terminate redis-server and the erlang server properly by committing
%% all possible states.
%%
%% @end
%%--------------------------------------------------------------------
-spec q() -> no_return().
q() ->
    ok = login_server:logout_all_players(),
    os:cmd("redis-cli shutdown"),
    init:stop().

%%--------------------------------------------------------------------
%% @doc
%% Connect observer node.
%%
%% @end
%%--------------------------------------------------------------------
-spec observer() -> pong | pang.
observer() ->
    net_adm:ping(?OBSERVER_NODE).

%%--------------------------------------------------------------------
%% @doc
%% This function is for skipping the implementation of from player initialization
%% stage and usually called by command exec function.
%%
%% @end
%%--------------------------------------------------------------------
-spec general_target(Uid, CommandContext) -> ok when
    Uid :: player_statem:uid(),
    CommandContext :: #command_context{}.
general_target(Uid, CommandContext) ->
    gen_statem:cast(Uid, {general_target, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% This is a general function for executing command in different states.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_command(Uid, CommandContext) -> ok when
    Uid :: player_statem:uid() | npc_statem:npc_uid() | scene_statem:scene_name(),
    CommandContext :: #command_context{}.
execute_command(Uid, CommandContext) ->
    gen_statem:cast(Uid, {execute_command, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% This is a general function for executing command in different states
%% for getting result.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_sync_command(ProcessName, CommandContext) -> Result when
    ProcessName :: player_statem:uid() | npc_statem:npc_uid() | scene_statem:scene_name(),
    CommandContext :: #command_context{},
    Result :: term(). % generic term
execute_sync_command(ProcessName, CommandContext) ->
    gen_statem:call(ProcessName, {execute_command, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% PKCS7 padding.
%%
%% @end
%%--------------------------------------------------------------------
-spec pkcs7pad(binary()) -> binary().
pkcs7pad(Bin) ->
    Diff = byte_size(Bin) rem 32,
    pkcs7pad(Bin, 32 - Diff).

%%--------------------------------------------------------------------
%% @doc
%% PKCS7 unpadding hardcoded.
%%
%% @end
%%--------------------------------------------------------------------
-spec pkcs7unpad(binary()) -> binary().
pkcs7unpad(<<>>) ->
    <<>>;
pkcs7unpad(Bin) ->
    Last = binary:last(Bin),
    Size = byte_size(Bin) - Last,

    case Bin of
        <<Data:Size/binary, 1>> -> Data;
        <<Data:Size/binary, 2, 2>> -> Data;
        <<Data:Size/binary, 3, 3, 3>> -> Data;
        <<Data:Size/binary, 4, 4, 4, 4>> -> Data;
        <<Data:Size/binary, 5, 5, 5, 5, 5>> -> Data;
        <<Data:Size/binary, 6, 6, 6, 6, 6, 6>> -> Data;
        <<Data:Size/binary, 7, 7, 7, 7, 7, 7, 7>> -> Data;
        <<Data:Size/binary, 8, 8, 8, 8, 8, 8, 8, 8>> -> Data;
        <<Data:Size/binary, 9, 9, 9, 9, 9, 9, 9, 9, 9>> -> Data;
        <<Data:Size/binary, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10>> -> Data;
        <<Data:Size/binary, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>> -> Data;
        <<Data:Size/binary, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>> -> Data;
        <<Data:Size/binary, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>> -> Data;
        <<Data:Size/binary, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>> -> Data;
        <<Data:Size/binary, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>> -> Data;
        <<Data:Size/binary, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16>> -> Data;
        <<Data:Size/binary, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17>> -> Data;
        <<Data:Size/binary, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18>> -> Data;
        <<Data:Size/binary, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19>> -> Data;
        <<Data:Size/binary, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20>> -> Data;
        <<Data:Size/binary, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21>> -> Data;
        <<Data:Size/binary, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22>> -> Data;
        <<Data:Size/binary, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23>> -> Data;
        <<Data:Size/binary, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24>> -> Data;
        <<Data:Size/binary, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25>> -> Data;
        <<Data:Size/binary, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26>> -> Data;
        <<Data:Size/binary, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27>> -> Data;
        <<Data:Size/binary, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28>> -> Data;
        <<Data:Size/binary, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29>> -> Data;
        <<Data:Size/binary, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30>> -> Data;
        <<Data:Size/binary, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31>> -> Data;
        <<Data:Size/binary, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32>> -> Data;
        _ -> erlang:error(bad_padding)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% PKCS7 padding hardcoded.
%%
%% @end
%%--------------------------------------------------------------------
-spec pkcs7pad(binary(), integer()) -> binary().
pkcs7pad(Bin, 1) ->
    <<Bin/binary, 1>>;
pkcs7pad(Bin, 2) ->
    <<Bin/binary, 2, 2>>;
pkcs7pad(Bin, 3) ->
    <<Bin/binary, 3, 3, 3>>;
pkcs7pad(Bin, 4) ->
    <<Bin/binary, 4, 4, 4, 4>>;
pkcs7pad(Bin, 5) ->
    <<Bin/binary, 5, 5, 5, 5, 5>>;
pkcs7pad(Bin, 6) ->
    <<Bin/binary, 6, 6, 6, 6, 6, 6>>;
pkcs7pad(Bin, 7) ->
    <<Bin/binary, 7, 7, 7, 7, 7, 7, 7>>;
pkcs7pad(Bin, 8) ->
    <<Bin/binary, 8, 8, 8, 8, 8, 8, 8, 8>>;
pkcs7pad(Bin, 9) ->
    <<Bin/binary, 9, 9, 9, 9, 9, 9, 9, 9, 9>>;
pkcs7pad(Bin, 10) ->
    <<Bin/binary, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10>>;
pkcs7pad(Bin, 11) ->
    <<Bin/binary, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>>;
pkcs7pad(Bin, 12) ->
    <<Bin/binary, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>;
pkcs7pad(Bin, 13) ->
    <<Bin/binary, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>>;
pkcs7pad(Bin, 14) ->
    <<Bin/binary, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>>;
pkcs7pad(Bin, 15) ->
    <<Bin/binary, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>>;
pkcs7pad(Bin, 16) ->
    <<Bin/binary, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16>>;
pkcs7pad(Bin, 17) ->
    <<Bin/binary, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17>>;
pkcs7pad(Bin, 18) ->
    <<Bin/binary, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18>>;
pkcs7pad(Bin, 19) ->
    <<Bin/binary, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19>>;
pkcs7pad(Bin, 20) ->
    <<Bin/binary, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20>>;
pkcs7pad(Bin, 21) ->
    <<Bin/binary, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21>>;
pkcs7pad(Bin, 22) ->
    <<Bin/binary, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22>>;
pkcs7pad(Bin, 23) ->
    <<Bin/binary, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23>>;
pkcs7pad(Bin, 24) ->
    <<Bin/binary, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24>>;
pkcs7pad(Bin, 25) ->
    <<Bin/binary, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25>>;
pkcs7pad(Bin, 26) ->
    <<Bin/binary, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26>>;
pkcs7pad(Bin, 27) ->
    <<Bin/binary, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27>>;
pkcs7pad(Bin, 28) ->
    <<Bin/binary, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28>>;
pkcs7pad(Bin, 29) ->
    <<Bin/binary, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29>>;
pkcs7pad(Bin, 30) ->
    <<Bin/binary, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30>>;
pkcs7pad(Bin, 31) ->
    <<Bin/binary, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31>>;
pkcs7pad(Bin, 32) ->
    <<Bin/binary, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32>>.