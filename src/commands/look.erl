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

-type sequence() :: pos_integer().
-type target() :: atom().

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
-spec exec(DispatcherPid, Uid, Args) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Args :: binary().
exec(DispatcherPid, Uid, Args) ->
    [RawTarget | Rest] = binary:split(Args, <<" ">>),
    Sequence = case Rest of
                   [] ->
                       1;
                   [RawSequence] ->
                       binary_to_integer(RawSequence)
               end,
    player_fsm:look_target(Uid, DispatcherPid, binary_to_atom(RawTarget, utf8), Sequence).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================