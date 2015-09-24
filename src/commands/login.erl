%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Login module
%%%
%%% @end
%%% Created : 26. Aug 2015 11:01 AM
%%%-------------------------------------------------------------------
-module(login).
-author("Shuieryin").

%% API
-export([exec/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Log user in by creating player_fsm and enters user's last logout scene.
%%
%% This function returns "ok" immeidately and the scene info will
%% be responsed to user from player_fsm by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    login_server:login(DispatcherPid, Uid).

%%%===================================================================
%%% Internal functions
%%%===================================================================