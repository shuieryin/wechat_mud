-module(direction).
%% API
-export([exec/3]).

%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 九月 2015 下午4:08
%%%-------------------------------------------------------------------
-author("shuieryin").

-type direction() :: east | south | west | north | northeast | southeast | southwest | northwest.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
