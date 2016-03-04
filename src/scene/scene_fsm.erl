%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Scene gen_fsm. This gen_fsm states linear as
%%% state_name
%%% This gen_fsm acts as template server and is initialized per csv
%%% file under priv/scene by scene_fsm_sup.erl which the number of initialized
%%% scene_fsm (server name is set as scene name) equals to the number
%%% of rows in scene csv file.
%%%
%%% @end
%%% Created : 19. Sep 2015 4:34 PM
%%%-------------------------------------------------------------------
-module(scene_fsm).
-author("shuieryin").

-behaviour(gen_fsm).

%% API
-export([
    start_link/1,
    scene_child_spec/4,
    enter/4,
    leave/2,
    show_scene/3,
    go_direction/3,
    scene_object_list/1,
    exits_map/1,
    general_target/1,
    morning/2,
    morning/3,
    player_quit/2,
    update_scene_info/2
]).

%% gen_fsm callbacks
-export([
    init/1,
    state_name/2,
    state_name/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    format_status/2
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/npc_profile.hrl").
-include("../data_type/player_profile.hrl").

-type scene_object() :: #simple_player{} | #simple_npc{}.
-type scene_character_name() :: npc_fsm:npc_id() | player_fsm:uid().
-type scene_name() :: atom(). % generic atom
-type exits_map() :: #{direction:directions() => nls_server:key()}.
-type exits_scenes() :: #{scene_name() => direction:directions()}.

-record(scene_state, {
    scene_info :: #scene_info{},
    scene_object_list :: [scene_object()],
    exits_scenes :: exits_scenes(),
    exits_description :: [nls_server:nls_object()]
}).

-type secene_state_name() :: morning | noon | afternoon | nightfall | night | midnight | dawn | state_name.

-export_type([
    scene_name/0,
    exits_map/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function starts the gen_fsm by setting scene name as server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(SceneInfo) -> gen:start_ret() when
    SceneInfo :: #scene_info{}.
start_link(
    #scene_info{
        id = SceneName
    } = SceneInfo
) ->
    gen_fsm:start_link({local, SceneName}, ?MODULE, SceneInfo, []).

%%--------------------------------------------------------------------
%% @doc
%% Generates scene fsm worker configs entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_child_spec(SceneValues, Restart, Shutdown, Type) -> SceneChild when
    SceneValues :: [csv_to_object:value()],
    Restart :: supervisor:restart(),
    Shutdown :: supervisor:shutdown(),
    Type :: supervisor:worker(),
    SceneChild :: scene_fsm_sup:scene_child().
scene_child_spec([_CityName | SceneValues], Restart, Shutdown, Type) ->
    [Verify | _RestSceneValues] = SceneValues,
    case Verify of
        undefined ->
            undefined;
        _Verify ->
            #scene_info{
                id = Id
            } = SceneInfo = list_to_tuple([scene_info | SceneValues]),
            {Id, {?MODULE, start_link, [SceneInfo]}, Restart, Shutdown, Type, [?MODULE]}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Enter a scene. When entering a scene, the scene information of
%% this scene is initially response to user and the player scene id
%% is then added to uid map of the scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec enter(SceneName, DispatcherPid, SimplePlayer, FromSceneName) -> ok when
    SceneName :: scene_name(),
    DispatcherPid :: pid(),
    SimplePlayer :: #simple_player{},
    FromSceneName :: scene_name().
enter(SceneName, DispatcherPid, SimplePlayer, FromSceneName) ->
    gen_fsm:send_all_state_event(SceneName, {enter, DispatcherPid, SimplePlayer, FromSceneName}).

%%--------------------------------------------------------------------
%% @doc
%% Go target direction. This function first checks whether the scene
%% name of target direction exists, if so, remove player uid from uid
%% list of scene state and returns the target scene name, or else
%% returns {undefined, SceneNlsServerName} but does nothing.
%%
%% @end
%%--------------------------------------------------------------------
-spec go_direction(SceneName, Uid, TargetDirection) -> Result when
    SceneName :: scene_name(),
    Uid :: player_fsm:uid(),
    TargetDirection :: direction:directions(),
    Result :: TargetSceneName | {undefined, SceneNlsServerName},
    TargetSceneName :: scene_name(),
    SceneNlsServerName :: erlang:registered_name().
go_direction(SceneName, Uid, TargetDirection) ->
    gen_fsm:sync_send_all_state_event(SceneName, {go_direction, Uid, TargetDirection}).

%%--------------------------------------------------------------------
%% @doc
%% Leaves scene by removing player uid from uid map of scene state.
%% This is usually called when player logging out.
%%
%% @end
%%--------------------------------------------------------------------
-spec leave(SceneName, Uid) -> ok when
    SceneName :: scene_name(),
    Uid :: player_fsm:uid().
leave(SceneName, Uid) ->
    gen_fsm:send_all_state_event(SceneName, {leave, Uid}).


%%--------------------------------------------------------------------
%% @doc
%% Display the current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_scene(CurSceneName, Uid, DispatcherPid) -> ok when
    CurSceneName :: scene_name(),
    DispatcherPid :: pid(),
    Uid :: player_fsm:uid().
show_scene(CurSceneName, Uid, DispatcherPid) ->
    gen_fsm:send_all_state_event(CurSceneName, {show_scene, Uid, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% This function is an aggregate function for executing action on target.
%%
%% The difference between target/1 is that no need to specify simulation
%% function on the current state event but uses common event "general_target".
%%
%% See target/1 for command details.
%% @see target/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec general_target(CommandContext) -> ok when
    CommandContext :: #command_context{}.
general_target(
    #command_context{
        scene = SceneName
    } = CommandContext
) ->
    gen_fsm:send_all_state_event(SceneName, {general_target, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene object list including players and npcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_object_list(CurSceneName) -> [scene_object()] when
    CurSceneName :: scene_name().
scene_object_list(CurSceneName) ->
    gen_fsm:sync_send_all_state_event(CurSceneName, scene_object_list).

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene exits map.
%%
%% @end
%%--------------------------------------------------------------------
-spec exits_map(CurSceneName) -> exits_map() when
    CurSceneName :: scene_name().
exits_map(CurSceneName) ->
    gen_fsm:sync_send_all_state_event(CurSceneName, exits_map).

%%--------------------------------------------------------------------
%% @doc
%% Notification from player process that is terminated and remove the
%% player from current scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec player_quit(SceneName, Uid) -> ok when
    SceneName :: scene_name(),
    Uid :: player_fsm:uid().
player_quit(SceneName, Uid) ->
    gen_fsm:send_all_state_event(SceneName, {player_quit, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Scene info hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_scene_info(SceneName, SceneInfo) -> ok when
    SceneName :: scene_name(),
    SceneInfo :: #scene_info{}.
update_scene_info(SceneName, SceneInfo) ->
    gen_fsm:sync_send_all_state_event(SceneName, {update_scene_info, SceneInfo}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% Initializes empty uid map in scene state.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(SceneInfo) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    SceneInfo :: #scene_info{},
    StateName :: secene_state_name(),
    StateData :: #scene_state{},
    Reason :: term(). % generic term
init(
    #scene_info{
        id = SceneName
    } = SceneInfo
) ->
    io:format("scene server ~p starting...", [SceneName]),

    State = populate_scene_state(SceneInfo, undefined),

    io:format("started~n"),
    {ok, morning, State}.

%%--------------------------------------------------------------------
%% @doc
%% Scene morning time.
%%
%% @end
%%--------------------------------------------------------------------
-spec morning(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event :: {execute_command, CommandContext},

    CommandContext :: #command_context{},

    State :: #scene_state{},
    NextStateName :: secene_state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
morning(
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, morning),
    {next_state, NextStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Scene morning time for sync event.
%%
%% @end
%%--------------------------------------------------------------------
-spec morning(Event, From, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {reply, Reply, NextStateName, NextState} |
    {reply, Reply, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} |
    {stop, Reason, Reply, NewState} when

    Event :: term(), % generic term
    Reply :: ok,

    From :: {pid(), term()}, % generic term
    State :: #scene_state{},
    NextStateName :: secene_state_name(),
    NextState :: State,
    Reason :: normal | term(), % generic term
    NewState :: State.
morning(
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    _From,
    State
) ->
    {ok, Result, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, morning),
    {reply, Result, NextStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec state_name(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event :: term(), % generic term
    State :: #scene_state{},
    NextStateName :: secene_state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec state_name(Event, From, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {reply, Reply, NextStateName, NextState} |
    {reply, Reply, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} |
    {stop, Reason, Reply, NewState} when

    Event :: term(), % generic term
    Reply :: ok,

    From :: {pid(), term()}, % generic term
    State :: #scene_state{},
    NextStateName :: secene_state_name(),
    NextState :: State,
    Reason :: normal | term(), % generic term
    NewState :: State.
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when

    Event ::
    {enter, DispatcherPid, SimplePlayer, FromSceneName} |
    {leave, Uid} |
    {look_scene, Uid, DispatcherPid} |
    {general_target, CommandContext} |
    {player_quit, Uid},

    SimplePlayer :: #simple_player{},
    FromSceneName :: scene_name(),
    DispatcherPid :: pid(),
    Uid :: player_fsm:uid(),
    CommandContext :: #command_context{},

    StateName :: secene_state_name(),
    StateData :: #scene_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_event(
    {
        enter,
        DispatcherPid,
        #simple_player{
            uid = Uid,
            name = PlayerName
        } = SimplePlayer,
        FromSceneName
    },
    StateName,
    #scene_state{
        scene_object_list = SceneObjectList,
        exits_scenes = ExitsScenes
    } = State
) ->
    {ok, IsPlayerExist} = do_show_scene(State, Uid, DispatcherPid),
    EnterSceneMessage = case FromSceneName of
                            undefined ->
                                [{nls, enter_scene, [PlayerName]}, <<"\n">>];
                            _FromSceneName ->
                                [{nls, enter_scene_from, [PlayerName, {nls, maps:get(FromSceneName, ExitsScenes)}]}, <<"\n">>]
                        end,
    UpdatedState =
        case IsPlayerExist of
            true ->
                State;
            false ->
                broadcast(State, EnterSceneMessage, scene, []),
                State#scene_state{scene_object_list = [SimplePlayer | SceneObjectList]}
        end,
    {next_state, StateName, UpdatedState};
handle_event(
    {leave, Uid},
    StateName,
    #scene_state{
        scene_object_list = SceneObjectList
    } = State
) ->
    #simple_player{name = PlayerName} = scene_player_by_uid(SceneObjectList, Uid),
    broadcast(State, [{nls, leave_scene, [PlayerName, {nls, unknown}]}, <<"\n">>], scene, [Uid]),
    {next_state, StateName, remove_scene_object(Uid, State)};
handle_event({show_scene, Uid, DispatcherPid}, StateName, State) ->
    {ok, _IsCallerExist} = do_show_scene(State, Uid, DispatcherPid),
    {next_state, StateName, State};
handle_event(
    {
        general_target,
        #command_context{
            from = #simple_player{
                uid = SrcUid
            },
            dispatcher_pid = DispatcherPid,
            target_name = TargetId,
            sequence = Sequence,
            target_name_bin = TargetBin
        } = CommandContext
    },
    StateName,
    #scene_state{
        scene_object_list = SceneObjectList
    } = State
) ->
    TargetSceneObject = grab_target_scene_objects(SceneObjectList, TargetId, Sequence),
    if
        undefined == TargetSceneObject ->
            ok = player_fsm:response_content(SrcUid, [{nls, no_such_target}, TargetBin, <<"\n">>], DispatcherPid);
        true ->
            UpdatedCommandContext = CommandContext#command_context{
                to = TargetSceneObject
            },

            TargetUid =
                case TargetSceneObject of
                    #simple_npc{
                        npc_uid = TargetNpcUid
                    } ->
                        TargetNpcUid;
                    #simple_player{
                        uid = TargetPlayerUid
                    } ->
                        TargetPlayerUid
                end,
            ok = cm:execute_command(TargetUid, UpdatedCommandContext)
    end,
    {next_state, StateName, State};
handle_event({player_quit, Uid}, StateName, State) ->
    {next_state, StateName, remove_scene_object(Uid, State)}.


%%--------------------------------------------------------------------
%% @doc
%% Filters scene objects by given target name.
%%
%% @end
%%--------------------------------------------------------------------
-spec grab_target_scene_objects(SceneObjectList, TargetCharacterName, Sequence) -> TargetSceneObject when
    SceneObjectList :: [scene_object()],
    TargetCharacterName :: scene_character_name(),
    Sequence :: look:sequence(),
    TargetSceneObject :: scene_object() | undefined.
grab_target_scene_objects(SceneObjectList, TargetCharacterName, Sequence) ->
    grab_target_scene_objects(SceneObjectList, TargetCharacterName, Sequence, 1).

%%--------------------------------------------------------------------
%% @doc
%% See parent function grab_target_scene_objects/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec grab_target_scene_objects(SceneObjectList, TargetName, Sequence, Counter) -> TargetSceneObject when
    SceneObjectList :: [scene_object()],
    TargetName :: scene_character_name(),
    Sequence :: look:sequence(),
    Counter :: pos_integer(), % generic integer
    TargetSceneObject :: scene_object() | undefined.
grab_target_scene_objects(
    [
        #simple_npc{
            npc_id = TargetNpcId
        } = SceneObject | _RestSceneObjectList
    ],
    TargetNpcId,
    Sequence,
    Sequence
) ->
    SceneObject;
grab_target_scene_objects(
    [
        #simple_player{
            id = TargetPlayerId
        } = SceneObject | _RestSceneObjectList
    ],
    TargetPlayerId,
    Sequence,
    Sequence
) ->
    SceneObject;
grab_target_scene_objects(
    [
        #simple_npc{
            npc_id = TargetNpcId
        } | RestSceneObjectList
    ],
    TargetNpcId,
    Sequence,
    Counter
) ->
    grab_target_scene_objects(RestSceneObjectList, TargetNpcId, Sequence, Counter + 1);
grab_target_scene_objects(
    [
        #simple_player{
            id = TargetPlayerId
        } | RestSceneObjectList
    ],
    TargetPlayerId,
    Sequence,
    Counter
) ->
    grab_target_scene_objects(RestSceneObjectList, TargetPlayerId, Sequence, Counter + 1);
grab_target_scene_objects([_SceneObject | RestSceneObjectList], TargetName, Sequence, Counter) ->
    grab_target_scene_objects(RestSceneObjectList, TargetName, Sequence, Counter);
grab_target_scene_objects([], _TargetName, _Sequence, _Counter) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Removes character info from state
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_scene_object(SceneObjectKey, State) -> UpdatedState when
    SceneObjectKey :: npc_fsm:npc_uid() | player_fsm:uid(),
    State :: #scene_state{},
    UpdatedState :: State.
remove_scene_object(
    SceneObjectKey,
    #scene_state{
        scene_object_list = SceneObjectList
    } = State
) ->
    State#scene_state{
        scene_object_list = lists:keydelete(SceneObjectKey, 2, SceneObjectList)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(Event, From, StateName, StateData) ->
    {reply, Reply, NextStateName, NewStateData} |
    {reply, Reply, NextStateName, NewStateData, timeout() | hibernate} |
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, Reply, NewStateData} |
    {stop, Reason, NewStateData} when

    Event :: {go_direction, Uid, TargetDirection} |
    scene_object_list |
    exits_map |
    {update_scene_info, SceneInfo},

    Reply :: SceneName | ok,

    Uid :: player_fsm:uid(),
    TargetDirection :: direction:directions(),
    SceneName :: scene_name(),
    SceneInfo :: #scene_info{},

    From :: {pid(), Tag :: term()}, % generic term
    StateName :: secene_state_name(),
    StateData :: #scene_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_sync_event(
    {go_direction, Uid, TargetDirection},
    _From,
    StateName,
    #scene_state{
        scene_info = #scene_info{
            exits = ExitsMap
        },
        scene_object_list = SceneObjectList
    } = State
) ->
    {TargetSceneName, UpdatedState} =
        case maps:get(TargetDirection, ExitsMap, undefined) of
            undefined ->
                {undefined, State};
            SceneName ->
                #simple_player{name = PlayerName} = scene_player_by_uid(SceneObjectList, Uid),
                broadcast(State, [{nls, leave_scene, [PlayerName, {nls, TargetDirection}]}, <<"\n">>], scene, [Uid]),
                {SceneName, remove_scene_object(Uid, State)}
        end,

    {reply, TargetSceneName, StateName, UpdatedState};
handle_sync_event(
    scene_object_list,
    _From,
    StateName,
    #scene_state{
        scene_object_list = SceneObjectList
    } = State
) ->
    {reply, SceneObjectList, StateName, State};
handle_sync_event(
    exits_map,
    _From,
    StateName,
    #scene_state{
        scene_info = #scene_info{
            exits = ExitsMap
        }
    } = State
) ->
    {reply, ExitsMap, StateName, State};
handle_sync_event({update_scene_info, NewSceneInfo}, _From, StateName, #scene_state{
    scene_object_list = ExistingSceneObjectList
}) ->
    UpdatedState = populate_scene_state(NewSceneInfo, ExistingSceneObjectList),
    {reply, ok, StateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when

    Info :: term(), % generic term
    StateName :: secene_state_name(),
    StateData :: #scene_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: normal | term(). % generic term
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, StateName, StateData) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(), % generic term
    StateName :: secene_state_name(),
    StateData :: #scene_state{}.
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData} when
    OldVsn :: term() | {down, term()}, % generic term
    StateName :: secene_state_name(),
    StateData :: #scene_state{},
    Extra :: term(), % generic term
    NextStateName :: secene_state_name(),
    NewStateData :: StateData.
code_change(_OldVsn, StateName, State, _Extra) ->
    try
        UpdatedState = temp_scene_data_update(State),
        {ok, StateName, UpdatedState}
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
            {ok, StateName, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is useful for customising the form and
%% appearance of the gen_server status for these cases.
%%
%% @spec format_status(Opt, StatusData) -> Status
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt, StatusData) -> Status when
    Opt :: 'normal' | 'terminate',
    StatusData :: [PDict | State],
    PDict :: [{Key :: term(), Value :: term()}], % generic term
    State :: #scene_state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_fsm:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene exits nls key list.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_exits_desc(ExitsMap) -> [ExitNls] when
    ExitsMap :: exits_map(),
    ExitNls :: [nls_server:nls_object()].
gen_exits_desc(ExitsMap) ->
    [[{nls, ExitKey}, <<" (">>, ExitKey, <<")\n">>] || ExitKey <- maps:keys(ExitsMap)].

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene player and npc name list without the caller player.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_characters_name_list(SceneObjectList, CallerUid) -> {IsCallerExist, CharacterNameList} when
    SceneObjectList :: [scene_object()],
    CallerUid :: player_fsm:uid(),
    CharacterNameList :: [nls_server:nls_object()],
    IsCallerExist :: boolean().
gen_characters_name_list(SceneObjectList, CallerUid) ->
    case length(SceneObjectList) of
        0 ->
            {false, []};
        _HasSceneObject ->
            gen_character_name(SceneObjectList, CallerUid, [<<"\n">>], false)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parent function see gen_characters_name_list/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_character_name(SceneObjectList, CallerUid, AccSceneObjectNameList, IsCallerExist) -> {UpdatedIsCallerExist, SceneObjectNameList} when
    SceneObjectList :: [scene_object()],
    CallerUid :: player_fsm:uid(),
    AccSceneObjectNameList :: [nls_server:nls_object()],
    SceneObjectNameList :: AccSceneObjectNameList,
    IsCallerExist :: boolean(),
    UpdatedIsCallerExist :: IsCallerExist.
gen_character_name([], _CallerUid, AccList, UpdatedIsCallerExist) ->
    SceneObjectNameList =
        case AccList of
            [<<"\n">>] ->
                [];
            _AccList ->
                AccList
        end,
    {UpdatedIsCallerExist, SceneObjectNameList};
gen_character_name(
    [
        #simple_npc{
            npc_id = NpcId,
            npc_name = NpcName
        } | Tail
    ],
    CallerUid,
    AccSceneObjectNameList,
    IsCallerExist
) ->
    NpcIdForDisplay = re:replace(NpcId, "_", " ", [global, {return, binary}]),
    gen_character_name(Tail, CallerUid, [NpcName, <<" (">>, NpcIdForDisplay, <<")">>, <<"\n">>] ++ AccSceneObjectNameList, IsCallerExist);
gen_character_name(
    [
        #simple_player{
            uid = CallerUid
        } | Tail
    ],
    CallerUid,
    AccCharactersNameList,
    _IsCallerExist
) ->
    gen_character_name(Tail, CallerUid, AccCharactersNameList, true);
gen_character_name(
    [
        #simple_player{
            name = PlayerName,
            id = Id
        } | Tail
    ],
    CallerUid,
    AccSceneObjectNameList,
    IsCallerExist
) ->
    gen_character_name(Tail, CallerUid, [PlayerName, <<" (">>, Id, <<")">>, <<"\n">>] ++ AccSceneObjectNameList, IsCallerExist).

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene info including scene title, scene description,
%% scene player name list, and exits.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_show_scene(State, Uid, DispatcherPid) -> {ok, IsCallerExist} when
    DispatcherPid :: pid(),
    State :: #scene_state{},
    Uid :: player_fsm:uid(),
    IsCallerExist :: boolean().
do_show_scene(
    #scene_state{
        scene_info = #scene_info{
            title = SceneTitle,
            desc = SceneDesc
        },
        scene_object_list = SceneObjectList,
        exits_description = ExitsDescription
    },
    Uid,
    DispatcherPid
) ->
    {IsCallerExist, SceneObjectNameList} = gen_characters_name_list(SceneObjectList, Uid),
    ContentList = lists:flatten([
        {nls, SceneTitle},
        <<"\n\n">>,
        {nls, SceneDesc},
        <<"\n\n">>,
        SceneObjectNameList,
        {nls, obvious_exits},
        <<"\n">>,
        ExitsDescription
    ]),
    ok = player_fsm:response_content(Uid, ContentList, DispatcherPid),
    {ok, IsCallerExist}.

%%--------------------------------------------------------------------
%% @doc
%% Broadcast message to current scene players except players
%% indicated in "ExceptUids".
%%
%% @end
%%--------------------------------------------------------------------
-spec broadcast(State, Message, MailType, ExceptUids) -> ok when
    State :: #scene_state{},
    Message :: player_fsm:mail_object(),
    MailType :: player_fsm:mail_type(),
    ExceptUids :: [player_fsm:uid()].
broadcast(
    #scene_state{
        scene_object_list = SceneObjectList
    },
    Message,
    MailType,
    ExceptUids
) ->
    lists:foreach(
        fun(SceneObject) ->
            case SceneObject of
                #simple_player{
                    uid = TargetPlayerUid
                } ->
                    IsBroadcast = not lists:member(TargetPlayerUid, ExceptUids),
                    if
                        IsBroadcast ->
                            player_fsm:append_message(TargetPlayerUid, Message, MailType);
                        true ->
                            do_nothing
                    end;
                _Other ->
                    do_nothing
            end
        end,
        SceneObjectList
    ).

%%--------------------------------------------------------------------
%% @doc
%% Get target scene player by uid.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_player_by_uid(SceneObjectList, TargetPlayerUid) -> Result when
    SceneObjectList :: [scene_object()],
    TargetPlayerUid :: player_fsm:uid(),
    Result :: #simple_player{} | undefined.
scene_player_by_uid(
    [
        #simple_player{
            uid = TargetPlayerUid
        } = SimplePlayer | _RestSceneObjectList
    ],
    TargetPlayerUid
) ->
    SimplePlayer;
scene_player_by_uid([_OtherSceneObject | Rest], TargetPlayerUid) ->
    scene_player_by_uid(Rest, TargetPlayerUid);
scene_player_by_uid([], _TargetPlayerUid) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Populate scene state for init and code change.
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_scene_state(#scene_info{}, ExistingSceneObjectList) -> #scene_state{} when
    ExistingSceneObjectList :: [scene_object()] | undefined.
populate_scene_state(#scene_info{
    npcs = NpcsSpec,
    exits = ExitsMap
} = SceneInfo, ExistingSceneObjectList) ->
    SceneNpcsList = case ExistingSceneObjectList of
                        undefined ->
                            npc_fsm_manager:new_npcs(NpcsSpec);
                        _Exist ->
                            NpcSize = length(npcs(ExistingSceneObjectList)),
                            if
                                NpcSize > 0 ->
                                    ExistingSceneObjectList;
                                true ->
                                    npc_fsm_manager:new_npcs(NpcsSpec) ++ ExistingSceneObjectList
                            end
                    end,

    ExitsScenes = maps:fold(
        fun(CurExit, CurSceneName, AccExitsScenes) ->
            AccExitsScenes#{CurSceneName => CurExit}
        end, #{}, ExitsMap),

    #scene_state{
        scene_info = SceneInfo,
        scene_object_list = SceneNpcsList,
        exits_scenes = ExitsScenes,
        exits_description = gen_exits_desc(ExitsMap)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Grab npcs of current scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec npcs(SceneObjectList) -> [#simple_npc{}] when
    SceneObjectList :: [scene_object()].
npcs(SceneObjectList) ->
    lists:filter(
        fun
            (#simple_npc{}) ->
                true;
            (_OtherObject) ->
                false
        end, SceneObjectList).

%%--------------------------------------------------------------------
%% @doc
%% Temporary code for handling data change for scene fsms.
%%
%% @end
%%--------------------------------------------------------------------
-spec temp_scene_data_update(State) -> UpdatedState when
    State :: tuple(), % generic tuple
    UpdatedState :: State.
temp_scene_data_update(State) ->
    State.