%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
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
%% Execute
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Lang) -> no_return() when
    Uid :: atom(),
    Lang :: atom(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid, Lang) ->
    case Lang of
        <<"all">> ->
            player_fsm:show_langs(DispatcherPid, Uid);
        _ ->
            player_fsm:switch_lang(DispatcherPid, Uid, Lang)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
