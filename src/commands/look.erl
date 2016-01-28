%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Look module. This module returns the current scene content to
%%% player when no arugments provided, or returns specific character
%%% or object vice versa.
%%%
%%% @end
%%% Created : 20. Sep 2015 8:19 PM
%%%-------------------------------------------------------------------
-module(look).
-author("shuieryin").

%% API
-export([
    exec/2,
    exec/3,
    being_look/3,
    feedback/3,
    look_scene/3
]).

-type sequence() :: pos_integer(). % generic integer

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").

-export_type([
    sequence/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the current scene content when no arguments provided.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    CommandContext = #command_context{
        command_func = look_scene,
        dispatcher_pid = DispatcherPid
    },
    cm:execute_command(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Show the first matched target scene object description.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, TargetArgs) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    TargetArgs :: binary().
exec(DispatcherPid, Uid, TargetArgs) ->
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    CommandContext = #command_context{
        command_func = being_look,
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        sequence = Sequence,
        target_name_bin = TargetArgs,
        self_targeted_message = [{nls, look_self}, <<"\n">>]
    },
    cm:general_target(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Command callback function for target player settlement.
%%
%% @end
%%--------------------------------------------------------------------
-spec being_look(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
being_look(
    #command_context{
        from = #simple_player{
            uid = SrcUid,
            name = SrcName
        }
    } = CommandContext,
    #player_state{
        self = #player_profile{
            uid = TargetPlayerUid,
            description = Description,
            self_description = SelfDescription
        }
    } = State,
    StateName
) ->
    TargetDescription = if
                            SrcUid == TargetPlayerUid ->
                                SelfDescription;
                            true ->
                                Description
                        end,

    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback,
        command_args = [TargetDescription, <<"\n">>]
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),

    SceneMessage = [SrcName, {nls, under_look}, <<"\n">>],
    UpdatedState = player_fsm:append_message_local(SceneMessage, scene, State),
    {ok, StateName, UpdatedState};
being_look(
    #command_context{
        from = #simple_player{
            uid = SrcUid
        }
    } = CommandContext,
    #npc_state{
        self = #npc_profile{
            character_desc = TargetDescription
        }
    } = State,
    StateName
) ->
    Message = [TargetDescription, <<"\n">>],

    UpdatedCommandContext = CommandContext#command_context{
        command_func = feedback,
        command_args = Message
    },
    ok = cm:execute_command(SrcUid, UpdatedCommandContext),
    {ok, StateName, State}.

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
feedback(
    #command_context{
        command_args = TargetDescription,
        dispatcher_pid = DispatcherPid
    },
    State,
    StateName
) ->
    UpdatedState = player_fsm:do_response_content(State, TargetDescription, DispatcherPid),
    {ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Display the current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_scene(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
look_scene(
    #command_context{
        dispatcher_pid = DispatcherPid
    },
    #player_state{
        self = #player_profile{
            scene = SceneName,
            uid = Uid
        }
    } = State,
    StateName
) ->
    scene_fsm:show_scene(SceneName, Uid, DispatcherPid),
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================