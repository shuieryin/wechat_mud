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
    exec/4,
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
-spec exec(DispatcherPid, Uid, RawInput, RestArgsBin) -> ok when
    Uid :: player_statem:uid(),
    RawInput :: binary(),
    DispatcherPid :: pid(),
    RestArgsBin :: binary().
exec(DispatcherPid, Uid, RawInput, Args) ->
    [TargetArgs, AffairName] = re:split(Args, <<"\s+about\s+">>),
    {ok, TargetId, Sequence} = elib:parse_target_id(TargetArgs),
    CommandContext = #command_context{
        raw_input = RawInput,
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
    StateName :: player_statem:player_state_name(),
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
                player_statem:do_response_content(State, SelfMessage, DispatcherPid),
                State;
            true ->
                UpdatedCommandContext = CommandContext#command_context{
                    command_func = answer,
                    scene = CurSceneName,
                    from = player_statem:simple_player(PlayerProfile),
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
    StateName :: player_statem:player_state_name() | npc_statem:npc_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
answer(
    #command_context{
        from = #simple_player{
            uid = SrcUid,
            name = FromName
        },
        command_args = #affair_context{
            affair_name = AffairName
        } = AffairContext
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
        command_args = AffairContext#affair_context{
            response_message = [{nls, dunno, [TargetName]}, <<"\n">>]
        }
    },
    AppendMessage = [{nls, ask_someone, [FromName, {nls, you}, AffairName]}, <<"\n">>, {nls, dunno, [{nls, you}]}, <<"\n">>],
    UpdatedState = player_statem:append_message_local(AppendMessage, scene, State),
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),
    {ok, StateName, UpdatedState};
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
    TargetAskNAnswer = elib:retrieve_n_break(
        fun(#ask_n_answer{affair_nls_values = AffairValues}) ->
            lists:member(AffairName, AffairValues)
        end, AskNAnswers),

    {UpdatedState, FinalStateName}
        = case TargetAskNAnswer of
              undefined ->
                  UpdatedCommandContext = CommandContext#command_context{
                      command_func = feedback,
                      command_args = AffairContext#affair_context{
                          response_message = [{nls, dunno, [NpcName]}, <<"\n">>]
                      }
                  },
                  ok = cm:execute_command(SrcUid, UpdatedCommandContext),
                  {State, StateName};
              #ask_n_answer{
                  affair_mod = AffairMod,
                  affair_func = AffairFunc
              } ->
                  UpdatedCommandContext = CommandContext#command_context{
                      command_func = feedback,
                      command_args = AffairContext#affair_context{
                          answer = TargetAskNAnswer
                      },
                      affair_mod_name = AffairMod
                  },
                  {UpdatedStateFromAffair, FinalCommandContext, UpdatedStateName} = AffairMod:AffairFunc(State, UpdatedCommandContext, StateName),
                  ok = cm:execute_command(SrcUid, FinalCommandContext),
                  {UpdatedStateFromAffair, UpdatedStateName}
          end,

    {ok, FinalStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Display answer back to player and execute subsequent logic.
%%
%% @end
%%--------------------------------------------------------------------
-spec feedback(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_statem:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
feedback(
    #command_context{
        command_args = #affair_context{
            affair_name = AffairName,
            response_message = Message
        },
        affair_mod_name = AffairMod,
        dispatcher_pid = DispatcherPid,
        to = Target
    } = CommandContext,
    State,
    StateName
) ->
    TargetName = case Target of
                     #simple_player{
                         name = PlayerName
                     } ->
                         PlayerName;
                     #simple_npc{
                         npc_name = NpcName
                     } ->
                         NpcName
                 end,

    FinalMessage = [{nls, ask_someone, [{nls, you}, TargetName, AffairName]}, <<"\n">> | Message],
    {UpdatedState, UpdatedStateName} = case AffairMod of
                                           undefined ->
                                               {State, StateName};
                                           _HasAffair ->
                                               AffairMod:feedback(State, CommandContext, StateName)
                                       end,
    FinalPlayerState = player_statem:do_response_content(UpdatedState, FinalMessage, DispatcherPid),

    {ok, UpdatedStateName, FinalPlayerState}.


%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================