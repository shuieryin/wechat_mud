%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Attack target. This will change both players state from
%%% non_battle to battle.
%%%
%%% @end
%%% Created : 16. Jan 2016 8:28 PM
%%%-------------------------------------------------------------------
-module(attack).
-author("shuieryin").

%% API
-export([
    exec/3,
    to_settle/3,
    feedback/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Attack target. This will change both players state from
%% non_battle to battle.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, TargetArgs) -> ok when
    DispatcherPid :: pid(),
    Uid :: player_fsm:uid(),
    TargetArgs :: binary().
exec(DispatcherPid, Uid, TargetArgs) ->
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    CommandContext = #command_context{
        command_func = from_init,
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        sequence = Sequence,
        target_name_bin = TargetArgs,
        self_targeted_message = [{nls, attack_self}, <<"\n">>]
    },
    cm:general_target(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for target player settlement.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_settle(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
to_settle(#command_context{from = #simple_player{uid = SrcUid, name = SrcName}} = CommandContext, State, StateName) ->
    Message = [{nls, under_attack, [SrcName]}],
    UpdatedState = player_fsm:append_message_local(Message, battle, State),

    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),

    {ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for feeding back to source player.
%%
%% @end
%%--------------------------------------------------------------------
-spec feedback(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
feedback(#command_context{dispatcher_pid = DispatcherPid, to = #simple_player{name = TargetName}}, State, StateName) ->
    Message = [{nls, launch_attack, [TargetName]}],
    UpdatedState = player_fsm:do_response_content(State, Message, DispatcherPid),
    {ok, StateName, UpdatedState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================