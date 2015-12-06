%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(player_fsm_test).
-author("shuieryin").

%% API
-export([
    command/1,
    initial_state/0,
    precondition/2,
    postcondition/3,
    next_state/3
]).

-export([
    test/1
]).

-define(SERVER, player_fsm).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_born_info.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

-record(state, {
    registered_uids_set,
    current_player_uid,
    current_scene,
    current_pid,
    valid_langs
}).

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    ?ST().

initial_state() ->
    Self = self(),
    RegisteredUidsSet = login_server:get_registered_player_uids(),
    CurrentPlayerUid = ?ONE_OF(gb_sets:to_list(RegisteredUidsSet)),
    ok = login_server:login(Self, CurrentPlayerUid),
    ValidLangs = cm:type_values(nls_server, support_lang),

    #state{
        registered_uids_set = RegisteredUidsSet,
        current_player_uid = CurrentPlayerUid,
        current_scene = player_fsm:current_scene_name(CurrentPlayerUid),
        current_pid = Self,
        valid_langs = ValidLangs
    }.

command(#state{current_player_uid = PlayerUid, current_scene = CurrentScene, valid_langs = ValidLangs}) ->
    Self = self(),
    SceneObjectList = scene_fsm:get_scene_object_list(CurrentScene),
    SceneObject = ?ONE_OF(SceneObjectList),
    TargetName = case SceneObject of
                     #simple_npc_fsm{npc_fsm_id = TargetNpcFsmId} ->
                         TargetNpcFsmId;
                     #simple_player{uid = TargetPlayerUid} ->
                         TargetPlayerUid;
                     _ ->
                         undefined
                 end,
    ExitsMap = scene_fsm:get_exits_map(CurrentScene),
    ExitNames = [invalid_direction_1, invalid_direction_2 | maps:keys(ExitsMap)],
    TargetDirection = ?ONE_OF(ExitNames),
    ValidLang = ?ONE_OF(ValidLangs),
    oneof([
        {call, ?SERVER, look_scene, [Self, PlayerUid]},
        {call, ?SERVER, switch_lang, [Self, PlayerUid, ValidLang]},
        {call, ?SERVER, look_target, [Self, PlayerUid, list_to_binary([<<"look ">>, atom_to_binary(TargetName, utf8)])]},
        {call, ?SERVER, go_direction, [Self, PlayerUid, TargetDirection]}
    ]).

next_state(#state{current_player_uid = PlayerUid, current_scene = CurrentScene, current_pid = Self} = ModelState, _Var, {call, ?SERVER, go_direction, [Self, PlayerUid, TargetDirection]}) ->
    case scene_fsm:get_exits_map(CurrentScene) of
        undefined ->
            ModelState;
        #{TargetDirection := TargetSceneName} ->
            io:format("next_state TargetSceneName:~p~nCurrentScene:~p~n", [TargetSceneName, CurrentScene]),
            ModelState#state{current_scene = TargetSceneName};
        Other ->
            io:format("next_state TargetDirection:~p~nOther:~p~n", [TargetDirection, Other]),
            ModelState
    end;
next_state(ModelState, _Var, {call, ?SERVER, _Action, _Args}) ->
    ModelState.

precondition(#state{current_player_uid = PlayerUid, current_scene = CurrentScene, current_pid = Self}, {call, ?SERVER, go_direction, [Self, PlayerUid, TargetDirection]}) ->
    if
        undefined == TargetDirection ->
            false;
        true ->
            case scene_fsm:get_exits_map(CurrentScene) of
                undefined ->
                    false;
                ExitsMap ->
                    maps:is_key(TargetDirection, ExitsMap)
            end
    end;
precondition(_ModelState, {call, ?SERVER, _Action, _Args}) ->
    true.

postcondition(#state{current_scene = CurrentSceneName, current_player_uid = PlayerUid, current_pid = Self}, {call, ?SERVER, go_direction, [Self, PlayerUid, _]}, Result) ->
    io:format("postcondition CurrentSceneName:~p~n", [CurrentSceneName]),
    CurrentSceneName == player_fsm:current_scene_name(PlayerUid) andalso Result == ok;
postcondition(_ModelState, {call, ?SERVER, look_target, _Args}, Result) ->
    Result == ok;
postcondition(_ModelState, {call, ?SERVER, switch_lang, _Args}, Result) ->
    Result == ok;
postcondition(_ModelState, {call, ?SERVER, look_scene, _Args}, Result) ->
    Result == ok;
postcondition(_ModelState, {call, ?SERVER, _Action, _Args}, _Result) ->
    true.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================