%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2015 4:08 PM
%%%-------------------------------------------------------------------
-module(direction).
-author("shuieryin").

%% API
-export([exec/3]).

-type direction() :: east | south | west | north | northeast | southeast | southwest | northwest | undefined.
-export_type([direction/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Execute
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Direction) -> no_return() when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Direction :: direction().
exec(DispatcherPid, Uid, Direction) ->
    case Direction of
        look ->
            player_fsm:look_scene(Uid, DispatcherPid);
        _ ->
            player_fsm:go_direction(Uid, DispatcherPid, Direction)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
