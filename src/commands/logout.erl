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
-export([
    exec/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Log user out by saving player profile to redis and remove player_statem.
%%
%% This function returns "ok" immediately and the scene info will
%% be respond to user from player_statem by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RawInput) -> ok when
    Uid :: player_statem:uid(),
    RawInput :: binary(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid, _RawInput) ->
    login_server:logout(DispatcherPid, Uid).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================