%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Re-register player module
%%%
%%% @end
%%% Created : 01. Sep 2015 11:28 PM
%%%-------------------------------------------------------------------
-module(rereg).
-author("Shuieryin").

%% API
-export([exec/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Re-register player. The existing player profile will not be removed
%% until the registration is done.
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
    login_server:register_uid(DispatcherPid, Uid).

%%%===================================================================
%%% Internal functions
%%%===================================================================