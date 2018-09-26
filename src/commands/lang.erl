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
-export([
    exec/4,
    switch_lang/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Execute language request. Input "all" shows all supported languages;
%% other inputs defaults to language switch.
%%
%% This function returns "ok" immeidately and the scene info will
%% be responsed to user from player_statem by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RawInput, RawTargetLang) -> ok when
    Uid :: player_statem:uid(),
    RawInput :: binary(),
    RawTargetLang :: binary(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid, RawInput, RawTargetLang) ->
    CurLang = player_statem:get_lang(Uid),
    case RawTargetLang of
        <<"all">> ->
            nls_server:show_langs(DispatcherPid, CurLang);
        _Args ->
            case nls_server:is_valid_lang(RawTargetLang) of
                true ->
                    CommandContext = #command_context{
                        raw_input = RawInput,
                        command_func = switch_lang,
                        command_args = binary_to_atom(RawTargetLang, utf8),
                        dispatcher_pid = DispatcherPid
                    },
                    cm:execute_command(Uid, CommandContext);
                false ->
                    {ok, _Pid} = player_statem:response_content(Uid, [{nls, invalid_lang}, RawTargetLang, <<"\n\n">>, {nls, lang_help}], DispatcherPid),
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Switches player language.
%%
%% @end
%%--------------------------------------------------------------------
-spec switch_lang(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_statem:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
switch_lang(
    #command_context{
        command_args = TargetLang,
        dispatcher_pid = DispatcherPid
    },
    #player_state{
        self = #player_profile{
            uid = Uid
        } = PlayerProfile
    } = State,
    StateName
) ->
    TargetLangMap = nls_server:lang_map(TargetLang),
    UpdatedState = player_statem:do_response_content(
        State#player_state{lang_map = TargetLangMap},
        [{nls, lang_switched}],
        DispatcherPid
    ),
    UpdatedPlayerProfile = PlayerProfile#player_profile{lang = TargetLang},
    ok = redis_client_server:async_set(Uid, UpdatedPlayerProfile, true),
    {
        ok,
        StateName,
        UpdatedState#player_state{
            self = UpdatedPlayerProfile
        }
    }.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
