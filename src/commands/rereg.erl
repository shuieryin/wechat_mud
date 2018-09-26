%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Re-register player module.
%%%
%%% @end
%%% Created : 01. Sep 2015 11:28 PM
%%%-------------------------------------------------------------------
-module(rereg).
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
%% Re-register player. The existing player profile will not be removed
%% until the registration is done.
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
exec(DispatcherPid, 'ogD_CvtfTf1fGpNV-dVrbgQ9I76c' = Uid, _RawInput) ->
    case whereis(Uid) of
        undefined ->
            login_server:register_uid(DispatcherPid, Uid);
        _Pid ->
            {ok, _Pid} = player_statem:response_content(Uid, [{nls, please_logout_first}], DispatcherPid),
            ok
    end;
exec(DispatcherPid, Uid, _RawInput) ->
    {ok, _Pid} = player_statem:response_content(Uid, [{nls, invalid_command}], DispatcherPid),
    ok.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================