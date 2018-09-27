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
-export([
    exec/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Log user in by creating player_statem and enters user's last logout scene.
%%
%% This function returns "ok" immeidately and the scene info will
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
    login_server:login(DispatcherPid, Uid).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================