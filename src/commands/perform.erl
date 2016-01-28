%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Perform skill on target.
%%%
%%% @end
%%% Created : 10. Jan 2016 8:50 PM
%%%-------------------------------------------------------------------
-module(perform).
-author("shuieryin").

%% API
-export([
    exec/3,
    from_init/3,
    to_settle/3,
    feedback/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Show the first matched target scene object description.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RestArgsBin) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    RestArgsBin :: binary().
exec(DispatcherPid, Uid, Args) ->
    [SkillId, TargetArgs] = re:split(Args, <<"\s+on\s+">>),
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    CommandContext = #command_context{
        command_func = from_init,
        command_args = SkillId,
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        sequence = Sequence,
        target_name_bin = TargetArgs,
        self_targeted_message = [{nls, attack_self}, <<"\n">>]
    },

    cm:execute_command(Uid, CommandContext).


%%--------------------------------------------------------------------
%% @doc
%% Command callback function for source player initialization.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_init(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
from_init(
    #command_context{
        self_targeted_message = SelfMessage,
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        command_args = SkillId
    } = CommandContext,

    #player_state{
        self = #player_profile{
            id = SrcPlayerId,
            scene = CurSceneName,
            battle_status = BattleStatus
        } = PlayerProfile,
        skill_map = SkillMap,
        battle_status_ri = BattleStatusRi
    } = State,
    StateName
) ->
    UpdatedState =
        if
            SrcPlayerId == TargetId ->
                player_fsm:do_response_content(State, SelfMessage, DispatcherPid);
            true ->
                case maps:get(SkillId, SkillMap, undefined) of
                    undefined ->
                        player_fsm:do_response_content(State, [{nls, no_such_skill, [SkillId]}], DispatcherPid);
                    #skill{
                        skill_formula = #skill_formula{
                            from_var_names = FromVarNames
                        }
                    } = Skill ->
                        ValueBindings = cm:collect_record_value(BattleStatusRi, BattleStatus, FromVarNames, erl_eval:new_bindings()),
                        UpdatedCommandContext = CommandContext#command_context{
                            command_func = to_settle,
                            scene = CurSceneName,
                            from = player_fsm:simple_player(PlayerProfile),
                            command_args = #perform_args{
                                value_bindings = ValueBindings,
                                skill = Skill
                            }
                        },
                        scene_fsm:general_target(UpdatedCommandContext),
                        State
                end
        end,

    {ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for target player settlement.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_settle(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{} | #npc_state{},
    StateName :: player_fsm:player_state_name() | npc_fsm:npc_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
to_settle(
    #command_context{
        from = #simple_player{
            name = SrcName
        }
    } = CommandContext,

    #player_state{
        self = #player_profile{
            battle_status = TargetBattleStatus
        } = TargetPlayerProfile,
        battle_status_ri = BattleStatusRi
    } = State,
    StateName
) ->
    {DamageValue, UpdatedTargetBattleStatus} = skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi),

    UnderAttackMessage = {nls, attack_under_attack_desc, [SrcName, {nls, unarmed}]},
    DamageMessage = {nls, damage_desc, [integer_to_binary(DamageValue)]},
    UpdatedState = player_fsm:append_message_local([UnderAttackMessage, <<"\n">>, DamageMessage, <<"\n">>], battle, State),

    {ok, StateName, UpdatedState#player_state{self = TargetPlayerProfile#player_profile{battle_status = UpdatedTargetBattleStatus}}};
to_settle(
    CommandContext,
    #npc_state{
        battle_status = TargetBattleStatus,
        battle_status_ri = BattleStatusRi
    } = State,
    StateName
) ->
    {_DamageValue, UpdatedTargetBattleStatus} = skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi),
    {
        ok,
        StateName,
        State#npc_state{
            battle_status = UpdatedTargetBattleStatus
        }
    }.

%%--------------------------------------------------------------------
%% @doc
%% Skill settlement.
%%
%% @end
%%--------------------------------------------------------------------
-spec skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi) -> {DamageValue, UpdatedTargetBattleStatus} when
    CommandContext :: #command_context{},
    TargetBattleStatus :: #battle_status{},
    BattleStatusRi :: [atom()], % generic atom
    DamageValue :: non_neg_integer(),
    UpdatedTargetBattleStatus :: TargetBattleStatus.
skill_calc(
    #command_context{
        command_args = #perform_args{
            skill = #skill{
                skill_formula = #skill_formula{
                    formula = SkillFormula,
                    to_var_names = ToVarNames
                }
            },
            value_bindings = ValueBindings
        } = PerformArgs,
        from = #simple_player{
            uid = SrcUid
        }
    } = CommandContext,
    #battle_status{
        'Hp' = TargetHp
    } = TargetBattleStatus,
    BattleStatusRi
) ->
    FinalBindings = cm:collect_record_value(BattleStatusRi, TargetBattleStatus, ToVarNames, ValueBindings),
    {value, RawDamageValue, _NewBindings} = erl_eval:exprs(SkillFormula, FinalBindings),
    DamageValue =
        if
            RawDamageValue < 0 ->
                0;
            true ->
                list_to_integer(float_to_list(RawDamageValue, [{decimals, 0}]))
        end,

    UpdatedTargetBattleStatus = TargetBattleStatus#battle_status{'Hp' = TargetHp - DamageValue},
    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback,
        command_args = PerformArgs#perform_args{
            damage_value = DamageValue
        }
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),
    {DamageValue, UpdatedTargetBattleStatus}.

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for feeding back to source player.
%%
%% @end
%%--------------------------------------------------------------------
-spec feedback(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{} | #npc_state{},
    StateName :: player_fsm:player_state_name() | npc_fsm:npc_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
feedback(
    #command_context{
        to = #simple_player{
            name = TargetName
        },
        command_args = #perform_args{
            damage_value = DamageValue
        },
        dispatcher_pid = DispatcherPid
    },
    State,
    StateName
) ->
    AttackMessage = {nls, attack_desc, [TargetName, {nls, unarmed}]},
    DamageMessage = {nls, damage_desc, [integer_to_binary(DamageValue)]},
    UpdatedState = player_fsm:do_response_content(State, [AttackMessage, <<"\n">>, DamageMessage, <<"\n">>], DispatcherPid),
    {ok, StateName, UpdatedState};
feedback(
    #command_context{
        to = #simple_npc{
            npc_name = TargetName
        },
        command_args = #perform_args{
            damage_value = DamageValue
        },
        dispatcher_pid = DispatcherPid
    },
    State,
    StateName
) ->
    AttackMessage = {nls, attack_desc, [TargetName, {nls, unarmed}]},
    DamageMessage = {nls, damage_desc, [integer_to_binary(DamageValue)]},
    UpdatedState = player_fsm:do_response_content(State, [AttackMessage, <<"\n">>, DamageMessage, <<"\n">>], DispatcherPid),
    {ok, StateName, UpdatedState}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================