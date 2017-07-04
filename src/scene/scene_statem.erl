%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Scene gen_statem. This gen_statem states linear as
%%% state_name
%%% This gen_statem acts as template server and is initialized per csv
%%% file under priv/scene by scene_statem_sup.erl which the number of initialized
%%% scene_statem (server name is set as scene name) equals to the number
%%% of rows in scene csv file.
%%%
%%% @end
%%% Created : 19. Sep 2015 4:34 PM
%%%-------------------------------------------------------------------
-module(scene_statem).
-author("shuieryin").

-behaviour(gen_statem).

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
    morning/3,
    update_scene_info/2,
    scene_state/1,
    update_state/1
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    format_status/2,
    callback_mode/0
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/npc_profile.hrl").
-include("../data_type/player_profile.hrl").

-type scene_state_name() :: morning | noon | afternoon | nightfall | night | midnight | dawn | state_name.
-type scene_object() :: #simple_player{} | #simple_npc{}.
-type scene_character_name() :: npc_statem:npc_id() | player_statem:uid().
-type scene_name() :: atom(). % generic atom
-type exits_map() :: #{direction:directions() => nls_server:key()}.
-type exits_scenes() :: #{scene_name() => direction:directions()}.

-record(scene_state, {
    scene_info :: #scene_info{},
    scene_object_list :: [scene_object()],
    exits_scenes :: exits_scenes(),
    exits_description :: [nls_server:nls_object()]
}).

-type action(Reply) :: gen_statem:reply_action() | {reply, gen_statem:from(), Reply}.
-type state_function_result(Reply) ::
gen_statem:event_handler_result(#scene_state{}) |
{keep_state_and_data, action(Reply)} |
{
    next_state,
    #scene_state{},
    scene_state_name(),
    action(Reply)
}.

-export_type([
    scene_name/0,
    exits_map/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function starts the gen_statem by setting scene name as server name.
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
    gen_statem:start_link({local, SceneName}, ?MODULE, SceneInfo, []).

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
    SceneChild :: scene_statem_sup:scene_child().
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
enter(SceneName, DispatcherPid, #simple_player{
    uid = Uid,
    name = PlayerName
} = SimplePlayer, FromSceneName) ->
    spawn(
        fun() ->
            #scene_state{
                scene_object_list = SceneObjectList,
                exits_scenes = ExitsScenes
            } = SceneData = scene_state(SceneName),
            {ok, IsPlayerExist} = do_show_scene(SceneData, Uid, DispatcherPid),
            EnterSceneMessage = case FromSceneName of
                                    undefined ->
                                        [{nls, enter_scene, [PlayerName]}, <<"\n">>];
                                    _FromSceneName ->
                                        [{nls, enter_scene_from, [PlayerName, {nls, maps:get(FromSceneName, ExitsScenes)}]}, <<"\n">>]
                                end,
            UpdatedState =
                case IsPlayerExist of
                    true ->
                        SceneData;
                    false ->
                        broadcast(SceneData, EnterSceneMessage, scene, []),
                        SceneData#scene_state{scene_object_list = [SimplePlayer | SceneObjectList]}
                end,
            update_state(UpdatedState)
        end),
    ok.

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
    Uid :: player_statem:uid(),
    TargetDirection :: direction:directions(),
    Result :: TargetSceneName | {undefined, SceneNlsServerName},
    TargetSceneName :: scene_name(),
    SceneNlsServerName :: erlang:registered_name().
go_direction(SceneName, Uid, TargetDirection) ->
    #scene_state{
        scene_info = #scene_info{
            exits = ExitsMap
        },
        scene_object_list = SceneObjectList
    } = SceneData = scene_state(SceneName),
    {TargetSceneName, UpdatedData} =
        case maps:get(TargetDirection, ExitsMap, undefined) of
            undefined ->
                {undefined, SceneData};
            RawSceneName ->
                #simple_player{name = PlayerName} = scene_player_by_uid(SceneObjectList, Uid),
                broadcast(SceneData, [{nls, leave_scene, [PlayerName, {nls, TargetDirection}]}, <<"\n">>], scene, [Uid]),
                {RawSceneName, remove_scene_object(Uid, SceneData)}
        end,

    update_state(UpdatedData),
    TargetSceneName.

%%--------------------------------------------------------------------
%% @doc
%% Leaves scene by removing player uid from uid map of scene state.
%% This is usually called when player logging out.
%%
%% @end
%%--------------------------------------------------------------------
-spec leave(SceneName, Uid) -> ok when
    SceneName :: scene_name(),
    Uid :: player_statem:uid().
leave(SceneName, Uid) ->
    spawn(
        fun() ->
            #scene_state{
                scene_object_list = SceneObjectList
            } = SceneData = scene_state(SceneName),
            #simple_player{name = PlayerName} = scene_player_by_uid(SceneObjectList, Uid),
            broadcast(SceneData, [{nls, leave_scene, [PlayerName, {nls, unknown}]}, <<"\n">>], scene, [Uid]),
            update_state(remove_scene_object(Uid, SceneData))
        end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Display the current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_scene(SceneName, Uid, DispatcherPid) -> ok when
    SceneName :: scene_name(),
    DispatcherPid :: pid(),
    Uid :: player_statem:uid().
show_scene(SceneName, Uid, DispatcherPid) ->
    spawn(
        fun() ->
            SceneData = scene_state(SceneName),
            {ok, _IsCallerExist} = do_show_scene(SceneData, Uid, DispatcherPid)
        end),
    ok.

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
        scene = SceneName,
        from = #simple_player{
            uid = SrcUid
        },
        dispatcher_pid = DispatcherPid,
        target_name = TargetId,
        sequence = Sequence,
        target_name_bin = TargetBin
    } = CommandContext
) ->
    spawn(
        fun() ->
            #scene_state{
                scene_object_list = SceneObjectList
            } = scene_state(SceneName),
            TargetSceneObject = grab_target_scene_objects(SceneObjectList, TargetId, Sequence),
            if
                undefined == TargetSceneObject ->
                    player_statem:response_content(SrcUid, [{nls, no_such_target}, TargetBin, <<"\n">>], DispatcherPid);
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
                    cm:execute_command(TargetUid, UpdatedCommandContext)
            end
        end),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene object list including players and npcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_object_list(CurSceneName) -> [scene_object()] when
    CurSceneName :: scene_name().
scene_object_list(CurSceneName) ->
    gen_statem:call(CurSceneName, scene_object_list).

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene exits map.
%%
%% @end
%%--------------------------------------------------------------------
-spec exits_map(CurSceneName) -> exits_map() when
    CurSceneName :: scene_name().
exits_map(CurSceneName) ->
    gen_statem:call(CurSceneName, exits_map).

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
    gen_statem:call(SceneName, {update_scene_info, SceneInfo}).

%%--------------------------------------------------------------------
%% @doc
%% Get scene state.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_state(scene_name()) -> #scene_state{}.
scene_state(SceneName) ->
    gen_statem:call(SceneName, scene_state).

-spec update_state(#scene_state{}) -> ok.
update_state(#scene_state{
    scene_info = #scene_info{
        id = SceneName
    }
} = SceneData) ->
    gen_statem:cast(SceneName, {update_state, SceneData}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
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
    StateName :: scene_state_name(),
    StateData :: #scene_state{},
    Reason :: term(). % generic term
init(
    #scene_info{
        id = SceneName
    } = SceneInfo
) ->
    io:format("scene server ~p starting...~n", [SceneName]),

    State = populate_scene_state(SceneInfo, undefined),

    io:format("~n~p started~n~n", [SceneName]),
    {ok, morning, State}.

%%--------------------------------------------------------------------
%% @doc
%% Scene morning time.
%%
%% @end
%%--------------------------------------------------------------------
-spec morning(EventType, EventContent, Data) -> state_function_result(Reply) when
    EventType :: gen_statem:event_type(),

    EventContent :: {execute_command, CommandContext} |
    scene_object_list |
    exits_map |
    {update_scene_info, SceneInfo} |
    scene_state |
    {update_state, Data},

    Reply :: SceneName | ok,

    Uid :: player_statem:uid(),
    SceneName :: scene_name(),
    SceneInfo :: #scene_info{},

    Uid :: player_statem:uid(),
    CommandContext :: #command_context{},
    Data :: #scene_state{}.
morning(
    cast,
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
    {next_state, NextStateName, UpdatedState};
morning(
    {call, _From},
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, Result, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, morning),
    {reply, Result, NextStateName, UpdatedState};
morning(
    {call, From},
    scene_object_list,
    #scene_state{
        scene_object_list = SceneObjectList
    }
) ->
    {keep_state_and_data, {reply, From, SceneObjectList}};
morning(
    {call, From},
    exits_map,
    #scene_state{
        scene_info = #scene_info{
            exits = ExitsMap
        }
    }
) ->
    {keep_state_and_data, {reply, From, ExitsMap}};
morning({call, From}, {update_scene_info, NewSceneInfo}, #scene_state{
    scene_object_list = ExistingSceneObjectList
}) ->
    UpdatedData = populate_scene_state(NewSceneInfo, ExistingSceneObjectList),
    {next_state, morning, UpdatedData, {reply, From, ok}};
morning({call, From}, scene_state, State) ->
    {keep_state_and_data, {reply, From, State}};
morning(cast, {update_state, SceneData}, _OldData) ->
    {next_state, morning, SceneData}.

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
    SceneObjectKey :: npc_statem:npc_uid() | player_statem:uid(),
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
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, StateName, StateData) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(), % generic term
    StateName :: scene_state_name(),
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
    StateName :: scene_state_name(),
    StateData :: #scene_state{},
    Extra :: term(), % generic term
    NextStateName :: scene_state_name(),
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
    gen_statem:format_status(Opt, StatusData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the callback mode to gen_statem
%%
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

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
    CallerUid :: player_statem:uid(),
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
    CallerUid :: player_statem:uid(),
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
    Uid :: player_statem:uid(),
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
    ok = player_statem:response_content(Uid, ContentList, DispatcherPid),
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
    Message :: player_statem:mail_object(),
    MailType :: player_statem:mail_type(),
    ExceptUids :: [player_statem:uid()].
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
                            player_statem:append_message(TargetPlayerUid, Message, MailType);
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
    TargetPlayerUid :: player_statem:uid(),
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
                            npc_statem_manager:new_npcs(NpcsSpec);
                        _Exist ->
                            new_npcs(NpcsSpec, ExistingSceneObjectList) ++ ExistingSceneObjectList
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
%% Create new npcs from spec.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npcs(NewNpcsSpec, ExistingSceneObjectList) -> [#simple_npc{}] when
    NewNpcsSpec :: [npc_statem_manager:npc_spec()],
    ExistingSceneObjectList :: [scene_object()].
new_npcs(NewNpcsSpec, ExistingSceneObjectList) ->
    DiffNpcsSpec =
        lists:foldr(
            fun({NpcName, _NpcAmount} = NewNpcSpec, AccDiffNpcsSpec) ->
                SceneNpcSize = length(scene_npcs_by_name([NpcName], ExistingSceneObjectList)),
                if
                    SceneNpcSize > 0 ->
                        AccDiffNpcsSpec;
                    true ->
                        [NewNpcSpec | AccDiffNpcsSpec]
                end
            end, [], NewNpcsSpec),

    case DiffNpcsSpec of
        [] ->
            [];
        _HasNewNpc ->
            npc_statem_manager:new_npcs(DiffNpcsSpec)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Grab npcs of current scene by name.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_npcs_by_name(NpcNames, ExistingSceneObjectList) -> [#simple_npc{}] when
    NpcNames :: [nls_server:key()],
    ExistingSceneObjectList :: [scene_object()].
scene_npcs_by_name(NpcNames, ExistingSceneObjectList) ->
    lists:filter(
        fun
            (#simple_npc{
                npc_id = NpcId
            }) ->
                lists:member(binary_to_atom(NpcId, utf8), NpcNames);
            (_OtherObject) ->
                false
        end, ExistingSceneObjectList).

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