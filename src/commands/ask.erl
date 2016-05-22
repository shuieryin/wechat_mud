%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Ask some body about something.
%%%
%%% @end
%%% Created : 21. May 2016 1:06 PM
%%%-------------------------------------------------------------------
-module(ask).
-author("shuieryin").

%% API
-export([
    exec/3,
    ask_init/3,
    answer/3,
    feedback/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Prepare ask command context.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RestArgsBin) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    RestArgsBin :: binary().
exec(DispatcherPid, Uid, Args) ->
    [TargetArgs, AffairName] = re:split(Args, <<"\s+about\s+">>),
    % TODO merge cm:parse_target_id into cm:general_target & cm:execute_command
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    CommandContext = #command_context{
        command_func = ask_init,
        command_args = #affair_context{
            affair_name = AffairName
        },
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        sequence = Sequence,
        target_name_bin = TargetArgs,
        self_targeted_message = [{nls, ask_self}, <<"\n">>]
    },

    cm:execute_command(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for source player initialization.
%%
%% @end
%%--------------------------------------------------------------------
-spec ask_init(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
ask_init(
    #command_context{
        self_targeted_message = SelfMessage,
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        command_args = AffairContext
    } = CommandContext,

    #player_state{
        self = #player_profile{
            id = SrcPlayerId,
            scene = CurSceneName
        } = PlayerProfile
    } = State,
    StateName
) ->
    UpdatedState =
        if
            SrcPlayerId == TargetId ->
                player_fsm:do_response_content(State, SelfMessage, DispatcherPid),
                State;
            true ->
                UpdatedCommandContext = CommandContext#command_context{
                    command_func = answer,
                    scene = CurSceneName,
                    from = player_fsm:simple_player(PlayerProfile),
                    command_args = AffairContext#affair_context{
                        from_player = PlayerProfile,
                        dispatcher_pid = DispatcherPid
                    }
                },
                scene_fsm:general_target(UpdatedCommandContext),
                State
        end,

    {ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Answer question.
%%
%% @end
%%--------------------------------------------------------------------
-spec answer(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{} | #npc_state{},
    StateName :: player_fsm:player_state_name() | npc_fsm:npc_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
answer(
    #command_context{
        from = #simple_player{
            uid = SrcUid
        }
    } = CommandContext,
    #player_state{
        self = #player_profile{
            name = TargetName
        }
    } = State,
    StateName
) ->
    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback,
        command_args = {undfined, TargetName}
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),
    {ok, StateName, State};
answer(
    #command_context{
        from = #simple_player{
            uid = SrcUid
        },
        command_args = #affair_context{
            affair_name = AffairName
        } = AffairContext
    } = CommandContext,
    #npc_state{
        self = #npc_profile{
            ask_n_answers = AskNAnswers,
            npc_name = NpcName
        }
    } = State,
    StateName
) ->
    TargetAskNAnswer = cm:retrieve_n_break(
        fun(#ask_n_answer{affair_nls_values = AffairValues}) ->
            lists:member(AffairName, AffairValues)
        end, AskNAnswers),

    UpdatedState = case TargetAskNAnswer of
                       undefined ->
                           UpdatedCommandContext = CommandContext#command_context{
                               command_func = feedback,
                               command_args = [{nls, dunno, [NpcName]}, <<"\n">>]
                           },
                           ok = cm:execute_command(SrcUid, UpdatedCommandContext),
                           State;
                       #ask_n_answer{
                           affair_mod = AffairMod,
                           affair_func = AffairFunc
                       } ->
                           UpdatedAffairContext = AffairContext#affair_context{
                               to_target = State,
                               answer = TargetAskNAnswer
                           },
                           AffairMod:AffairFunc(UpdatedAffairContext)
                   end,

    {ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Display answer back to player and execute subsequent logic.
%%
%% @end
%%--------------------------------------------------------------------
-spec feedback(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
feedback(
    #command_context{
        command_args = Message,
        dispatcher_pid = DispatcherPid
    },
    State,
    StateName
) ->
    player_fsm:do_response_content(State, Message, DispatcherPid),
    {ok, StateName, State}.


%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================