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
    execute_sync_command/2
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
    rb:stop(),
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

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================