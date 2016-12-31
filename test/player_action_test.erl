%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(player_action_test).
-author("shuieryin").

-import(wechat_mud_SUITE, [mod_input/2]).

%% API
-export([
    test/1
]).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_profile.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

-record(state, {
    player_uid,
    player_id,
    pid,
    valid_langs,
    func_total_weighing
}).

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    Self = self(),
    LoggedInUidsSet = login_server:logged_in_player_uids(),
    CurrentPlayerUid = ?ONE_OF(gb_sets:to_list(LoggedInUidsSet)),
    case whereis(CurrentPlayerUid) of
        undefined ->
            true;
        _Exist ->
            ValidLangs = elib:type_values(nls_server, support_lang),

            RandomFuncs = [
                {100, fun look_scene/1},
                {100, fun look_target/1},
                {50, fun look_target_invalid/1},
                {250, fun go_direction/1},
                {100, fun hp/1},
                {100, fun lang/1},
                {50, fun lang_invalid/1},
                {50, fun lang_invalid_1/1},
                {300, fun perform/1},
                {50, fun perform_invalid/1},
                {50, fun perform_invalid_1/1},
                {50, fun attack/1},
                {50, fun attack_invalid/1},
                {300, fun ask/1},
                {50, fun ask_invalid/1},
                {50, fun rereg/1},
                {50, fun invalid_input/1},
                {30, fun player_id/1},
                {30, fun player_state/1},
                {30, fun player_state_by_id/1},
                {30, fun mail_box/1},
                {30, fun lang_map/1},
                {50, fun upgrade_value_by_id/1},
                {50, fun lang_all/1},
                {50, fun exits_map/1}
            ],

            ModelState = #state{
                player_uid = CurrentPlayerUid,
                player_id = register_test:test_id(atom_to_binary(CurrentPlayerUid, utf8)),
                pid = Self,
                valid_langs = ValidLangs,
                func_total_weighing = elib:total_weighing(RandomFuncs)
            },

            run_test(RandomFuncs, ModelState)
    end.

run_test(RandomFuncs, #state{func_total_weighing = TotalWeighing} = ModelState) ->
    {_LeftWeighing, TargetFunc} = elib:rand_by_weigh(TotalWeighing, RandomFuncs),
    apply(TargetFunc, [ModelState]),
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
look_scene(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, ?ONE_OF([<<"l">>, <<"look">>, <<"5">>])).

look_target_invalid(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"look afsdf">>).

look_target(#state{player_uid = PlayerUid}) ->
    TargetName = try

                     CurrentSceneName = player_statem:current_scene_name(PlayerUid),
                     SceneObjectList = scene_fsm:scene_object_list(CurrentSceneName),
                     SceneObject = ?ONE_OF(SceneObjectList),
                     atom_to_binary(case SceneObject of
                                        #simple_npc{npc_uid = TargetNpcFsmId} ->
                                            TargetNpcFsmId;
                                        #simple_player{uid = TargetPlayerUid} ->
                                            TargetPlayerUid
                                    end, utf8)
                 catch
                     Type:Reason ->
                         error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
                 end,

    case TargetName of
        ok ->
            ok;
        _Success ->
            mod_input(PlayerUid, <<"look ", TargetName/binary>>)
    end.

go_direction(#state{player_uid = PlayerUid}) ->
    ExitNames = [
        <<"invalid_direction_1">>,
        <<"invalid_direction_2">>,
        <<"n">>,
        <<"e">>,
        <<"s">>,
        <<"w">>,
        <<"ne">>,
        <<"se">>,
        <<"sw">>,
        <<"nw">>,
        <<"1">>,
        <<"2">>,
        <<"3">>,
        <<"4">>,
        <<"6">>,
        <<"7">>,
        <<"8">>,
        <<"9">>,
        <<"north">>,
        <<"south">>,
        <<"east">>,
        <<"west">>,
        <<"northeast">>,
        <<"southeast">>,
        <<"southwest">>,
        <<"northwest">>
    ],
    TargetDirection = ?ONE_OF(ExitNames),

    mod_input(PlayerUid, TargetDirection).

hp(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"hp">>).

lang_all(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"lang all">>).

lang_invalid(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"lang">>).

lang_invalid_1(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"lang pq">>).

lang(#state{player_uid = PlayerUid, valid_langs = ValidLangs}) ->
    ValidLang2 = atom_to_binary(?ONE_OF(ValidLangs), utf8),
    mod_input(PlayerUid, <<"lang ", ValidLang2/binary>>).

perform_invalid(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"perform">>).

perform_invalid_1(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"perform sjkdls on ajklsd">>).

perform(#state{player_uid = PlayerUid}) ->
    Command = try
                  IdList = scene_targets(PlayerUid),
                  CommandContent = <<"perform ${skill} on ${target_id}">>,
                  Skills = elib:type_values(player_statem, skills),
                  SkillName = atom_to_binary(?ONE_OF([invalid_skill | Skills]), utf8),
                  nls_server:fill_in_content(CommandContent, [SkillName, perform_convert(?ONE_OF(IdList))], <<>>)
              catch
                  Type:Reason ->
                      error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
              end,
    case Command of
        ok ->
            ok;
        _Success ->
            mod_input(PlayerUid, Command)
    end.

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
            TargetId;
        _Other ->
            <<"undefined">>
    end.

attack_invalid(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"attack">>).

attack(#state{player_uid = PlayerUid}) ->
    Command = try
                  TargetId = perform_convert(?ONE_OF(scene_targets(PlayerUid))),
                  <<<<"attack ">>/binary, TargetId/binary>>
              catch
                  Type:Reason ->
                      error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
              end,
    case Command of
        ok ->
            ok;
        _Success ->
            mod_input(PlayerUid, Command)
    end.

scene_targets(PlayerUid) ->
    CurrentSceneName = player_statem:current_scene_name(PlayerUid),
    SceneObjectList = scene_fsm:scene_object_list(CurrentSceneName),

    SceneObjList = [SceneObject || SceneObject <- SceneObjectList, perform_filter(SceneObject)],
    %ct:pal("Scene object list:~p~n", [SceneObjList]),
    SceneObjList.

ask_invalid(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"ask">>).

ask(#state{player_uid = PlayerUid}) ->
    Command = try
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
                  <<<<"ask ">>/binary, TargetId/binary, " about ", Affair/binary>>
              catch
                  Type:Reason ->
                      error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
              end,

    case Command of
        ok ->
            ok;
        _Success ->
            mod_input(PlayerUid, Command)
    end.

rereg(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"rereg">>).

invalid_input(#state{player_uid = PlayerUid}) ->
    mod_input(PlayerUid, <<"afsdf">>).

player_id(#state{player_uid = PlayerUid}) ->
    try
        player_statem:player_id(PlayerUid)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

player_state(#state{player_uid = PlayerUid}) ->
    try
        player_statem:player_state(PlayerUid)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

player_state_by_id(#state{player_id = PlayerId}) ->
    try
        player_statem:player_state_by_id(PlayerId)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

mail_box(#state{player_uid = PlayerUid}) ->
    try
        player_statem:mail_box(PlayerUid)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

lang_map(#state{player_uid = PlayerUid}) ->
    try
        player_statem:lang_map(PlayerUid)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

upgrade_value_by_id(#state{player_id = PlayerId}) ->
    try
        player_statem:upgrade_value_by_id(PlayerId, 2)
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.

exits_map(_State) ->
    try
        scene_fsm:exits_map(?ONE_OF([dream_board_nw, dream_board_w, dream_board_sw, dream_board_s, dream_board_se, dream_board_e, dream_board_ne, dream_board_n, dream_board_center]))
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()])
    end.