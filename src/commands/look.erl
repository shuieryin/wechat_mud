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
    exec/3]).

-type sequence() :: pos_integer(). % generic integer
-type target() :: atom(). % generic atom

-export_type([sequence/0,
    target/0]).

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
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    player_fsm:look_scene(Uid, DispatcherPid).

%%--------------------------------------------------------------------
%% @doc
%% Show the first matched target scene object description.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RestArgsBin) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    RestArgsBin :: binary().
exec(DispatcherPid, Uid, Args) ->
    player_fsm:look_target(Uid, DispatcherPid, Args).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================