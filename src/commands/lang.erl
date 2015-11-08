%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Language setting module
%%%
%%% @end
%%% Created : 20. Sep 2015 8:19 PM
%%%-------------------------------------------------------------------
-module(lang).
-author("shuieryin").

%% API
-export([exec/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Execute language request. Input "all" shows all supported languages;
%% other inputs defaults to language switch.
%%
%% This function returns "ok" immeidately and the scene info will
%% be responsed to user from player_fsm by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Lang) -> ok when
    Uid :: atom(),
    Lang :: atom(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid, Lang) ->
    case Lang of
        <<"all">> ->
            nls_server:show_langs(DispatcherPid, player_fsm:get_lang(Uid));
        _ ->
            player_fsm:switch_lang(DispatcherPid, Uid, Lang)
    end.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
