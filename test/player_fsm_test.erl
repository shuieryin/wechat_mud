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
    ok = login_server:login(Self, CurrentPlayerUid),
    ValidLangs = cm:type_values(nls_server, support_lang),

    ModelState = #state{
        player_uid = CurrentPlayerUid,
        pid = Self,
        valid_langs = ValidLangs
    },

    RandomFuncs = [
        fun() ->
            look_scene(ModelState)
        end,

        fun() ->
            switch_lang(ModelState)
        end,

        fun() ->
            look_target(ModelState)
        end,

        fun() ->
            go_direction(ModelState)
        end
    ],

    ?assert(proper:quickcheck(?FORALL(_L, integer(), run_test(RandomFuncs)), 50)).

run_test(RandomFuncs) ->
    ok == apply(?ONE_OF(RandomFuncs), []).

%%%===================================================================
%%% Internal functions
%%%===================================================================
look_scene(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, player_fsm:look_scene(Self, PlayerUid)).

switch_lang(#state{pid = Self, player_uid = PlayerUid, valid_langs = ValidLangs}) ->
    ?assertMatch(ok, player_fsm:switch_lang(Self, PlayerUid, ?ONE_OF(ValidLangs))).

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
    ?assertMatch(ok, player_fsm:look_target(Self, PlayerUid, list_to_binary([<<"look ">>, atom_to_binary(TargetName, utf8)]))).

go_direction(#state{pid = Self, player_uid = PlayerUid}) ->
    CurrentSceneName = player_fsm:current_scene_name(PlayerUid),
    ExitsMap = scene_fsm:get_exits_map(CurrentSceneName),
    ExitNames = [invalid_direction_1, invalid_direction_2 | maps:keys(ExitsMap)],
    TargetDirection = ?ONE_OF(ExitNames),

    ?assertMatch(ok, player_fsm:go_direction(Self, PlayerUid, TargetDirection)),
    case maps:get(TargetDirection, ExitsMap, undefined) of
        undefined ->
            ?assertMatch(CurrentSceneName, player_fsm:current_scene_name(PlayerUid));
        TargetSceneName ->
            ?assertMatch(TargetSceneName, player_fsm:current_scene_name(PlayerUid))
    end.