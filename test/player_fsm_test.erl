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
    test/1
]).

-define(SERVER, player_fsm).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_born_info.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

-record(state, {
    player_uid,
    pid,
    valid_langs
}).

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    Self = self(),
    RegisteredUidsSet = login_server:get_registered_player_uids(),
    CurrentPlayerUid = ?ONE_OF(gb_sets:to_list(RegisteredUidsSet)),
    ValidLangs = cm:type_values(nls_server, support_lang),

    ModelState = #state{
        player_uid = CurrentPlayerUid,
        pid = Self,
        valid_langs = ValidLangs
    },

    RandomFuncs = [
        fun look_scene/1,
        fun look_target/1,
        fun go_direction/1,
        fun hp/1,
        fun lang/1
    ],

    ?assert(proper:quickcheck(?FORALL(_L, integer(), run_test(RandomFuncs, ModelState)), 50)),
    logout:exec(Self, CurrentPlayerUid).

run_test(RandomFuncs, ModelState) ->
    apply(?ONE_OF(RandomFuncs), [ModelState]),
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
look_scene(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, look:exec(Self, PlayerUid)).

look_target(#state{pid = Self, player_uid = PlayerUid}) ->
    CurrentSceneName = player_fsm:current_scene_name(PlayerUid),
    SceneObjectList = scene_fsm:get_scene_object_list(CurrentSceneName),
    SceneObject = ?ONE_OF(SceneObjectList),
    TargetName = case SceneObject of
                     #simple_npc_fsm{npc_fsm_id = TargetNpcFsmId} ->
                         TargetNpcFsmId;
                     #simple_player{uid = TargetPlayerUid} ->
                         TargetPlayerUid
                 end,
    ?assertMatch(ok, look:exec(Self, PlayerUid, list_to_binary([<<"look ">>, atom_to_binary(TargetName, utf8)]))).

go_direction(#state{pid = Self, player_uid = PlayerUid}) ->
    CurrentSceneName = player_fsm:current_scene_name(PlayerUid),
    ExitsMap = scene_fsm:get_exits_map(CurrentSceneName),
    ExitNames = [invalid_direction_1, invalid_direction_2 | maps:keys(ExitsMap)],
    TargetDirection = ?ONE_OF(ExitNames),

    ?assertMatch(ok, direction:exec(Self, PlayerUid, TargetDirection)),
    case maps:get(TargetDirection, ExitsMap, undefined) of
        undefined ->
            ?assertMatch(CurrentSceneName, player_fsm:current_scene_name(PlayerUid));
        TargetSceneName ->
            ?assertMatch(TargetSceneName, player_fsm:current_scene_name(PlayerUid))
    end.

hp(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, hp:exec(Self, PlayerUid)).

lang(#state{pid = Self, player_uid = PlayerUid, valid_langs = ValidLangs}) ->
    ?assertMatch(ok, lang:exec(Self, PlayerUid, ?ONE_OF([<<"all">>, atom_to_binary(?ONE_OF(ValidLangs), utf8), <<"kr">>]))).