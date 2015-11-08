%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Look module. This module returns the current scene content to
%%% player when no arugments provided, or returns specific character
%%% or object vice versa.
%%%
%%% @end
%%% Created : 20. Sep 2015 8:19 PM
%%%-------------------------------------------------------------------
-module(look).
-author("shuieryin").

%% API
-export([exec/2,
    exec/3,
    exec/4]).

-type sequence() :: pos_integer().
-type target() :: {npc_fsm_manager:npc_type(), sequence()} | atom().

-export_type([target/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the current scene content when no arguments provided.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    player_fsm:look_scene(Uid, DispatcherPid).

%%--------------------------------------------------------------------
%% @doc
%% Show the first matched target scene object description.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Target) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Target :: target().
exec(DispatcherPid, Uid, Target) ->
    player_fsm:look_target(Uid, DispatcherPid, Target, 1).

%%--------------------------------------------------------------------
%% @doc
%% Show the matched target scene object description by sequence.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Target, Sequence) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Target :: target(),
    Sequence :: sequence().
exec(DispatcherPid, Uid, Target, Sequence) ->
    player_fsm:look_target(Uid, DispatcherPid, Target, Sequence).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
