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

-type perform_result() :: {
    from | to, % FromTo
    atom(), % FieldName % generic atom
    term(), % FieldValue % generic term
    term(), % ChangedValue % generic term
    {
        nls_server:key(), % ActiveNlsKey
        nls_server:key() % PassiveNlsKey
    } | nls_server:key() % SingleNlsKey
}.

-export_type([
    perform_result/0
]).

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
    CommandContext,
    #player_state{
        self = #player_profile{
            battle_status = TargetBattleStatus
        } = TargetPlayerProfile,
        battle_status_ri = BattleStatusRi
    } = State,
    StateName
) ->
    {FinalMessages, UpdatedTargetBattleStatus} = skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi),
    UpdatedState = player_fsm:append_message_local(FinalMessages, battle, State),
    {ok, StateName, UpdatedState#player_state{self = TargetPlayerProfile#player_profile{battle_status = UpdatedTargetBattleStatus}}};
to_settle(
    CommandContext,
    #npc_state{
        battle_status = TargetBattleStatus,
        battle_status_ri = BattleStatusRi
    } = State,
    StateName
) ->
    {_FinalMessages, UpdatedTargetBattleStatus} = skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi),
    {
        ok,
        StateName,
        State#npc_state{
            battle_status = UpdatedTargetBattleStatus
        }
    }.

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
        }
    } = CommandContext,
    State,
    StateName
) ->
    {ok, StateName, handle_feedback(TargetName, CommandContext, State)};
feedback(
    #command_context{
        to = #simple_npc{
            npc_name = TargetName
        }
    } = CommandContext,
    State,
    StateName
) ->
    {ok, StateName, handle_feedback(TargetName, CommandContext, State)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Skill settlement.
%%
%% @end
%%--------------------------------------------------------------------
-spec skill_calc(CommandContext, TargetBattleStatus, BattleStatusRi) -> {FinalMessages, UpdatedTargetBattleStatus} when
    CommandContext :: #command_context{},
    TargetBattleStatus :: #battle_status{},
    BattleStatusRi :: [atom()], % generic atom
    FinalMessages :: [nls_server:nls_object()],
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
            uid = SrcUid,
            name = SrcName
        }
    } = CommandContext,
    TargetBattleStatus,
    BattleStatusRi
) ->
    FinalBindings = cm:collect_record_value(BattleStatusRi, TargetBattleStatus, ToVarNames, ValueBindings),
    {value, PerformResults, _NewBindings} = erl_eval:exprs(SkillFormula, FinalBindings),

    {FinalMessages, FinalCalcValueBindings} = handle_perform_results(to, SrcName, PerformResults, [], []),

    UpdatedTargetBattleStatus = cm:update_record_value(BattleStatusRi, TargetBattleStatus, FinalCalcValueBindings),

    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback,
        command_args = PerformArgs#perform_args{
            perform_results = PerformResults
        }
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),
    {lists:reverse(FinalMessages), UpdatedTargetBattleStatus}.

%%--------------------------------------------------------------------
%% @doc
%% Handle perform results to determine the message content to show
%% and battle status to change.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_perform_results(BaseFromTo, SrcName, PerformResults, AccMessages, AccCalcValueBindings) -> {FinalMessages, FinalCalcValueBindings} when
    BaseFromTo :: from | to,
    SrcName :: player_fsm:name(),
    PerformResults :: [perform_result()],
    AccMessages :: [player_fsm:mail_object()],
    AccCalcValueBindings :: erl_eval:expressions(),
    FinalMessages :: player_fsm:mail_object(),
    FinalCalcValueBindings :: AccCalcValueBindings.
handle_perform_results(BaseFromTo, RawSrcName, [{FromTo, FieldName, FieldValue, ChangedValue, NlsKeys} | RestPerformResults], AccMessages, AccCalcValueBindings) ->
    {NlsKey, SrcName} = case NlsKeys of
                            {ActiveNls, PassiveNls} ->
                                case BaseFromTo of
                                    from ->
                                        {ActiveNls, RawSrcName};
                                    to ->
                                        {PassiveNls, RawSrcName}
                                end;
                            SingleNls ->
                                case BaseFromTo of
                                    from ->
                                        {SingleNls, {nls, you}};
                                    to ->
                                        {SingleNls, RawSrcName}
                                end
                        end,

    UpdatedAccCalcValueBindings =
        case FromTo of
            BaseFromTo ->
                erl_eval:add_binding(FieldName, FieldValue, AccCalcValueBindings);
            _NotBase ->
                AccCalcValueBindings
        end,

    Message = [{nls, NlsKey, [SrcName, ChangedValue]}, <<"\n">>],

    handle_perform_results(BaseFromTo, SrcName, RestPerformResults, [Message | AccMessages], UpdatedAccCalcValueBindings);
handle_perform_results(_BaseFromTo, _SrcName, [], FinalMessages, FinalCalcValueBindings) ->
    {lists:flatten(lists:reverse(FinalMessages)), FinalCalcValueBindings}.

%%--------------------------------------------------------------------
%% @doc
%% Handle perform results to determine the message content to show
%% and battle status to change for feedback.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_feedback(TargetName, CommandContext, State) -> UpdatedState when
    TargetName :: player_fsm:name() | npc_fsm:npc_name(),
    CommandContext :: #command_context{},
    State :: #player_state{},
    UpdatedState :: State.
handle_feedback(
    TargetName,
    #command_context{
        command_args = #perform_args{
            perform_results = PerformResults
        },
        dispatcher_pid = DispatcherPid
    },
    #player_state{
        self = #player_profile{
            battle_status = BattleStatus
        } = PlayerProfile,
        battle_status_ri = BattleStatusRi
    } = State
) ->
    {FinalMessages, FinalCalcValueBindings} = handle_perform_results(from, TargetName, PerformResults, [], []),
    UpdatedTargetBattleStatus = cm:update_record_value(BattleStatusRi, BattleStatus, FinalCalcValueBindings),
    UpdatedState = player_fsm:do_response_content(State, FinalMessages, DispatcherPid),
    UpdatedState#player_state{
        self = PlayerProfile#player_profile{
            battle_status = UpdatedTargetBattleStatus
        }
    }.
