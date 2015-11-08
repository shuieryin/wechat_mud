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
%% Convert from <<"little boy 2">> to "Target=little_boy" and "Sequence=2".
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Args) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Args :: binary().
exec(DispatcherPid, Uid, Args) ->
    [RawSequence | Rest] = lists:reverse(string:tokens(binary_to_list(Args), " ")),
    {Target, Sequence} =
        case re:run(RawSequence, "^[0-9]*$") of
            {match, _} ->
                {list_to_atom(string:join(lists:reverse(Rest), "_")), list_to_integer(RawSequence)};
            _ ->
                {list_to_atom(re:replace(Args, <<" ">>, <<"_">>, [global, {return, list}])), 1}
        end,
    player_fsm:look_target(Uid, DispatcherPid, Target, Sequence).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================