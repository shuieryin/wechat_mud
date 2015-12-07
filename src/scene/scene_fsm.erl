%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Scene gen_fsm. This gen_fsm states linear as
%%% state_name
%%% This gen_fsm acts as template server and is initialized per csv
%%% file under priv/scene by scene_sup.erl which the number of initialized
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
    enter/5,
    leave/2,
    go_direction/3,
    look_scene/3,
    look_target/4,
    get_scene_object_list/1,
    get_exits_map/1
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
-include("../data_type/npc_born_info.hrl").
-include("../data_type/player_profile.hrl").

-type scene_object() :: #simple_player{} | #simple_npc_fsm{}.
-type scene_character_name() :: npc_fsm_manager:npc_type() | player_fsm:uid().
-type scene_name() :: atom(). % generic atom
-type exit() :: north | east | south | west | northeast | southeast | southwest | northwest.
-type exits_map() :: #{exit() => nls_server:key()}.

-record(state, {
    scene_info :: #scene_info{},
    scene_object_list :: [scene_object()]
}).

-type state_name() :: state_name.

-export_type([scene_name/0,
    exits_map/0]).

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
start_link(#scene_info{id = SceneName} = SceneInfo) ->
    gen_fsm:start_link({local, SceneName}, ?MODULE, SceneInfo, []).

%%--------------------------------------------------------------------
%% @doc
%% Enter a scene. When entering a scene, the scene information of
%% this scene is initially response to user and the player scene id
%% is then added to uid map of the scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec enter(SceneName, Uid, PlayerName, Id, DispatcherPid) -> ok when
    SceneName :: scene_name(),
    Uid :: player_fsm:uid(),
    PlayerName :: player_fsm:name(),
    Id :: player_fsm:id(),
    DispatcherPid :: pid().
enter(SceneName, Uid, PlayerName, Id, DispatcherPid) ->
    gen_fsm:send_all_state_event(SceneName, {enter, Uid, PlayerName, Id, DispatcherPid}).

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
%% Looks at current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_scene(CurSceneName, Uid, DispatcherPid) -> ok when
    CurSceneName :: scene_name(),
    DispatcherPid :: pid(),
    Uid :: player_fsm:uid().
look_scene(CurSceneName, Uid, DispatcherPid) ->
    gen_fsm:send_all_state_event(CurSceneName, {look_scene, Uid, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Looks at current scene by response current scene info to player.
%% LookArgs is converted from
%%        binary "little boy 2" to "Target=little_boy" and "Sequence=2".
%%
%% @end
%%--------------------------------------------------------------------
-spec look_target(CurSceneName, SrcSimplePlayer, DispatcherPid, LookArgs) -> ok when
    CurSceneName :: scene_name(),
    DispatcherPid :: pid(),
    SrcSimplePlayer :: #simple_player{},
    LookArgs :: binary().
look_target(CurSceneName, SrcSimplePlayer, DispatcherPid, LookArgs) ->
    gen_fsm:send_all_state_event(CurSceneName, {look_target, SrcSimplePlayer, DispatcherPid, LookArgs}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene object list including players and npcs.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_scene_object_list(CurSceneName) -> [scene_object()] when
    CurSceneName :: scene_name().
get_scene_object_list(CurSceneName) ->
    gen_fsm:sync_send_all_state_event(CurSceneName, get_scene_object_list).

%%--------------------------------------------------------------------
%% @doc
%% Gets the current scene exits map.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_exits_map(CurSceneName) -> exits_map() when
    CurSceneName :: scene_name().
get_exits_map(CurSceneName) ->
    gen_fsm:sync_send_all_state_event(CurSceneName, get_exits_map).

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
    StateName :: state_name(),
    StateData :: #state{},
    Reason :: term(). % generic term
init(#scene_info{id = SceneName, npcs = NpcsSpec} = SceneInfo) ->
    io:format("scene server ~p starting...", [SceneName]),

    SceneNpcFsmList = npc_fsm_manager:new_npcs(NpcsSpec),
    State = #state{scene_info = SceneInfo, scene_object_list = SceneNpcFsmList},

    io:format("started~n"),
    {ok, state_name, State}.

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
    State :: #state{},
    NextStateName :: state_name(),
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
    State :: #state{},
    NextStateName :: state_name(),
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
    {enter, Uid, PlayerName, Id, DispatcherPid} |
    {leave, Uid} |
    {look_scene, Uid, DispatcherPid} |
    {look_target, SrcSimplePlayer, DispatcherPid, LookArgs},

    PlayerName :: player_fsm:name(),
    Id :: player_fsm:id(),
    DispatcherPid :: pid(),
    SrcSimplePlayer :: #simple_player{},
    Uid :: player_fsm:uid(),
    LookArgs :: binary(),

    StateName :: state_name(),
    StateData :: #state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_event({enter, Uid, PlayerName, Id, DispatcherPid}, StateName, #state{scene_object_list = SceneObjectList} = State) ->
    ok = show_scene(State, Uid, DispatcherPid),
    {next_state, StateName, State#state{scene_object_list = [#simple_player{uid = Uid, name = PlayerName, id = Id} | SceneObjectList]}};
handle_event({leave, Uid}, StateName, State) ->
    {next_state, StateName, remove_scene_object(Uid, State)};
handle_event({look_scene, Uid, DispatcherPid}, StateName, State) ->
    ok = show_scene(State, Uid, DispatcherPid),
    {next_state, StateName, State};
handle_event({look_target, #simple_player{uid = Uid} = SrcSimplePlayer, DispatcherPid, LookArgs}, StateName, #state{scene_object_list = SceneObjectList} = State) ->
    [RawSequence | Rest] = lists:reverse(re:split(LookArgs, <<" ">>)),
    {Target, Sequence} =
        case Rest of
            [] ->
                {binary_to_atom(RawSequence, utf8), 1};
            _ ->
                case re:run(RawSequence, "^[0-9]*$") of
                    {match, _} ->
                        {binary_to_atom(cm:binary_join(lists:reverse(Rest), <<"_">>), utf8), binary_to_integer(RawSequence)};
                    _ ->
                        {binary_to_atom(re:replace(LookArgs, <<" ">>, <<"_">>, [global, {return, binary}]), utf8), 1}
                end
        end,
    TargetSceneObject = grab_target_scene_objects(SceneObjectList, Target, Sequence),
    ContentList = case TargetSceneObject of
                      undefined ->
                          [{nls, no_such_target}, LookArgs, <<"\n">>];
                      #simple_npc_fsm{npc_fsm_id = TargetNpcFsmId} ->
                          npc_fsm:being_looked(TargetNpcFsmId, Uid);
                      #simple_player{uid = TargetPlayerUid} ->
                          player_fsm:being_looked(TargetPlayerUid, SrcSimplePlayer)
                  end,
    player_fsm:response_content(Uid, ContentList, DispatcherPid),
    {next_state, StateName, State}.

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
grab_target_scene_objects([#simple_npc_fsm{npc_type = TargetNpcType} = SceneObject | _], TargetNpcType, Sequence, Sequence) ->
    SceneObject;
grab_target_scene_objects([#simple_player{id = TargetPlayerId} = SceneObject | _], TargetPlayerId, Sequence, Sequence) ->
    SceneObject;
grab_target_scene_objects([#simple_npc_fsm{npc_type = TargetNpcType} | Tail], TargetNpcType, Sequence, Counter) ->
    grab_target_scene_objects(Tail, TargetNpcType, Sequence, Counter + 1);
grab_target_scene_objects([#simple_player{id = TargetPlayerId} | Tail], TargetPlayerId, Sequence, Counter) ->
    grab_target_scene_objects(Tail, TargetPlayerId, Sequence, Counter + 1);
grab_target_scene_objects([_ | Tail], TargetName, Sequence, Counter) ->
    grab_target_scene_objects(Tail, TargetName, Sequence, Counter);
grab_target_scene_objects([], _, _, _) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Removes character info from state
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_scene_object(SceneObjectKey, State) -> UpdatedState when
    SceneObjectKey :: npc_fsm_manager:npc_fsm_id() | player_fsm:uid(),
    State :: #state{},
    UpdatedState :: State.
remove_scene_object(SceneObjectKey, #state{scene_object_list = SceneObjectList} = State) ->
    State#state{scene_object_list = lists:keydelete(SceneObjectKey, 2, SceneObjectList)}.

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
    get_scene_object_list |
    get_exits_map,
    Reply :: SceneName,

    Uid :: player_fsm:uid(),
    TargetDirection :: direction:directions(),
    SceneName :: scene_name(),

    From :: {pid(), Tag :: term()}, % generic term
    StateName :: state_name(),
    StateData :: #state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_sync_event({go_direction, Uid, TargetDirection}, _From, StateName, #state{scene_info = #scene_info{exits = ExitsMap}} = State) ->
    {TargetSceneName, UpdatedState} =
        case maps:get(TargetDirection, ExitsMap, undefined) of
            undefined ->
                {undefined, State};
            SceneName ->
                {SceneName, remove_scene_object(Uid, State)}
        end,

    {reply, TargetSceneName, StateName, UpdatedState};
handle_sync_event(get_scene_object_list, _From, StateName, #state{scene_object_list = SceneObjectList} = State) ->
    {reply, SceneObjectList, StateName, State};
handle_sync_event(get_exits_map, _From, StateName, #state{scene_info = #scene_info{exits = ExitsMap}} = State) ->
    {reply, ExitsMap, StateName, State}.

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
    StateName :: state_name(),
    StateData :: #state{},
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
    StateName :: state_name(),
    StateData :: #state{}.
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
    StateName :: state_name(),
    StateData :: #state{},
    Extra :: term(), % generic term
    NextStateName :: state_name(),
    NewStateData :: StateData.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

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
    State :: #state{},
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
    [[{nls, ExitKey}, <<"\n">>] || ExitKey <- maps:keys(ExitsMap)].

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene player and npc name list without the caller player.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_characters_name_list(SceneObjectList, CallerUid) -> CharacterNameList when
    SceneObjectList :: [scene_object()],
    CallerUid :: player_fsm:uid(),
    CharacterNameList :: [nls_server:nls_object()].
gen_characters_name_list(SceneObjectList, CallerUid) ->
    case length(SceneObjectList) of
        0 ->
            [];
        _ ->
            gen_character_name(SceneObjectList, CallerUid, [<<"\n">>])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parent function see gen_characters_name_list/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_character_name(SceneObjectList, CallerUid, AccSceneObjectNameList) -> SceneObjectNameList when
    SceneObjectList :: [scene_object()],
    CallerUid :: player_fsm:uid(),
    AccSceneObjectNameList :: [nls_server:nls_object()],
    SceneObjectNameList :: AccSceneObjectNameList.
gen_character_name([], _, AccList) ->
    case AccList of
        [<<"\n">>] ->
            [];
        _ ->
            AccList
    end;
gen_character_name([#simple_npc_fsm{npc_type = NpcType, npc_name_nls_key = NpcNameNlsKey} | Tail], CallerUid, AccSceneObjectNameList) ->
    NpcTypeForDisplay = re:replace(atom_to_list(NpcType), "_", " ", [global, {return, binary}]),
    gen_character_name(Tail, CallerUid, [{nls, NpcNameNlsKey}, <<" (">>, NpcTypeForDisplay, <<")">>, <<"\n">>] ++ AccSceneObjectNameList);
gen_character_name([#simple_player{uid = CallerUid} | Tail], CallerUid, AccCharactersNameList) ->
    gen_character_name(Tail, CallerUid, AccCharactersNameList);
gen_character_name([#simple_player{name = PlayerName, id = Id} | Tail], CallerUid, AccSceneObjectNameList) ->
    gen_character_name(Tail, CallerUid, [PlayerName, <<" (">>, atom_to_binary(Id, utf8), <<")">>, <<"\n">>] ++ AccSceneObjectNameList).

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene info including scene title, scene description,
%% scene player name list, and exits.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_scene(State, Uid, DispatcherPid) -> ok when
    DispatcherPid :: pid(),
    State :: #state{},
    Uid :: player_fsm:uid().
show_scene(#state{scene_info = #scene_info{exits = ExitsMap, title = SceneTitle, desc = SceneDesc}, scene_object_list = SceneObjectList}, Uid, DispatcherPid) ->
    ContentList = lists:flatten([
        {nls, SceneTitle},
        <<"\n\n">>,
        {nls, SceneDesc},
        <<"\n\n">>,
        gen_characters_name_list(SceneObjectList, Uid),
        {nls, obvious_exits},
        <<"\n">>,
        gen_exits_desc(ExitsMap)
    ]),
    player_fsm:response_content(Uid, ContentList, DispatcherPid).