%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2015 11:01 AM
%%%-------------------------------------------------------------------
-module(logout).
-author("Shuieryin").

%% API
-export([exec/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Log user out by saving player profile to redis and remove player_fsm.
%%
%% This function returns "ok" immeidately and the scene info will
%% be responsed to user from player_fsm by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid) -> ok when
    Uid :: player_statem:uid(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    login_server:logout(DispatcherPid, Uid).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================