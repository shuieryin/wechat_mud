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
-spec exec(DispatcherPid, Uid, RawTargetLang) -> ok when
    Uid :: atom(),
    RawTargetLang :: binary(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid, RawTargetLang) ->
    CurLang = player_fsm:get_lang(Uid),
    TargetLang = binary_to_atom(RawTargetLang, utf8),
    case TargetLang of
        all ->
            nls_server:show_langs(DispatcherPid, CurLang);
        _ ->
            case nls_server:is_valid_lang(TargetLang) of
                true ->
                    player_fsm:switch_lang(DispatcherPid, Uid, TargetLang);
                _ ->
                    nls_server:response_content(commands, [{nls, invalid_lang}, atom_to_binary(TargetLang, utf8), <<"\n\n">>, {nls, lang_help}], CurLang, DispatcherPid)
            end
    end.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
