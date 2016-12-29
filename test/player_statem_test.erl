%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%% TODO implement black box testings by sending commands from another state machine
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(player_statem_test).
-author("shuieryin").

%% API
-export([
    test/1
]).

-define(SERVER, player_statem).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_profile.hrl").
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
    LoggedInUidsSet = login_server:logged_in_player_uids(),
    CurrentPlayerUid = ?ONE_OF(gb_sets:to_list(LoggedInUidsSet)),
    ValidLangs = elib:type_values(nls_server, support_lang),

    ModelState = #state{
        player_uid = CurrentPlayerUid,
        pid = Self,
        valid_langs = ValidLangs
    },

    RandomFuncs = [
        {fun look_scene/1, 1},
        {fun look_target/1, 1},
        {fun go_direction/1, 10},
        {fun hp/1, 1},
        {fun lang/1, 1},
        {fun perform/1, 1},
        {fun attack/1, 1},
        {fun ask/1, 1},
        {fun rereg/1, 1}
    ],

    ?assert(proper:quickcheck(?FORALL(_L, integer(), run_test(RandomFuncs, ModelState)), 3000)),
    IsLogout = rand:normal() > 0,
    ct:pal("IsLogout:~p~n", [IsLogout]),
    if
        IsLogout ->
            logout:exec(Self, CurrentPlayerUid, <<"CTEST">>);
        true ->
            ok
    end.

run_test(RandomFuncs, ModelState) ->
    {Func, Times} = ?ONE_OF(RandomFuncs),
    lists:foreach(
        fun(_Index) ->
            apply(Func, [ModelState])
        end,
        lists:seq(1, Times)
    ),
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
look_scene(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, look:exec(Self, PlayerUid, <<"CTEST">>)).

look_target(#state{pid = Self, player_uid = PlayerUid}) ->
    CurrentSceneName = player_statem:current_scene_name(PlayerUid),
    SceneObjectList = scene_fsm:scene_object_list(CurrentSceneName),
    SceneObject = ?ONE_OF(SceneObjectList),
    TargetName = case SceneObject of
                     #simple_npc{npc_uid = TargetNpcFsmId} ->
                         TargetNpcFsmId;
                     #simple_player{uid = TargetPlayerUid} ->
                         TargetPlayerUid
                 end,
    ?assertMatch(ok, look:exec(Self, PlayerUid, <<"CTEST">>, list_to_binary([<<"look ">>, atom_to_binary(TargetName, utf8)]))).

go_direction(#state{pid = Self, player_uid = PlayerUid}) ->
    CurrentSceneName = player_statem:current_scene_name(PlayerUid),
    ExitsMap = scene_fsm:exits_map(CurrentSceneName),
    ExitNames = [invalid_direction_1, invalid_direction_2 | maps:keys(ExitsMap)],
    TargetDirection = ?ONE_OF(ExitNames),

    ?assertMatch(ok, direction:exec(Self, PlayerUid, <<"CTEST">>, TargetDirection)).

hp(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, hp:exec(Self, PlayerUid, <<"CTEST">>)).

lang(#state{pid = Self, player_uid = PlayerUid, valid_langs = ValidLangs}) ->
    ?assertMatch(ok, lang:exec(Self, PlayerUid, <<"CTEST">>, ?ONE_OF([<<"all">>, atom_to_binary(?ONE_OF(ValidLangs), utf8), <<"kr">>]))).

perform(#state{pid = Self, player_uid = PlayerUid}) ->
    IdList = scene_targets(PlayerUid),
    CommandContent = <<"perform ${skill} on ${target_id}">>,
    Skills = elib:type_values(player_statem, skills),
    SkillName = atom_to_binary(?ONE_OF(Skills), utf8),
    Command = nls_server:fill_in_content(CommandContent, [SkillName, perform_convert(?ONE_OF(IdList))], <<>>),
    ?assertMatch(ok, perform:exec(Self, PlayerUid, <<"CTEST">>, Command)).

perform_filter(Elem) ->
    case Elem of
        #simple_player{} ->
            true;
        #simple_npc{} ->
            true
%%        _ ->
%%            false
    end.

perform_convert(Elem) ->
    case Elem of
        #simple_player{id = TargetId} ->
            TargetId;
        #simple_npc{npc_id = TargetId} ->
            TargetId
    end.

attack(#state{pid = Self, player_uid = PlayerUid}) ->
    TargetId = perform_convert(?ONE_OF(scene_targets(PlayerUid))),
    ct:pal("TargetId:~p~n", [TargetId]),
    Command = <<<<"attack ">>/binary, TargetId/binary>>,
    ?assertMatch(ok, attack:exec(Self, PlayerUid, <<"CTEST">>, Command)).

scene_targets(PlayerUid) ->
    CurrentSceneName = player_statem:current_scene_name(PlayerUid),
    SceneObjectList = scene_fsm:scene_object_list(CurrentSceneName),

    SceneObjList = [SceneObject || SceneObject <- SceneObjectList, perform_filter(SceneObject)],
    %ct:pal("Scene object list:~p~n", [SceneObjList]),
    SceneObjList.

ask(#state{pid = Self, player_uid = PlayerUid}) ->
    {TargetId, Affair} =
        case ?ONE_OF(scene_targets(PlayerUid)) of
            #simple_player{
                id = PlayerId
            } ->
                {PlayerId, <<"hihi">>};
            #simple_npc{
                npc_uid = NpcUid,
                npc_id = NpcId
            } ->
                #npc_state{
                    self = #npc_profile{
                        ask_n_answers = RawAskNAnswers
                    }
                } = npc_fsm:npc_state(NpcUid),

                ReturnAffairName =
                    case RawAskNAnswers of
                        [] ->
                            <<"hihi">>;
                        AskNAnswers ->
                            #ask_n_answer{
                                affair_func = AffairFunc
                            } = ?ONE_OF(AskNAnswers),
                            atom_to_binary(AffairFunc, utf8)
                    end,

                {NpcId, ReturnAffairName}
        end,
    Command = <<<<"ask ">>/binary, TargetId/binary, " about ", Affair/binary>>,
    ?assertMatch(ok, ask:exec(Self, PlayerUid, <<"CTEST">>, Command)).

rereg(#state{pid = Self, player_uid = PlayerUid}) ->
    ?assertMatch(ok, rereg:exec(Self, PlayerUid, <<"CTEST">>)).