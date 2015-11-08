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
-export([start_link/1,
    stop/0,
    enter/4,
    leave/2,
    go_direction/5,
    look_scene/4,
    look_target/6]).

%% gen_fsm callbacks
-export([init/1,
    state_name/2,
    state_name/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    format_status/2]).

-include("../nls/nls.hrl").

-define(SCENE_NLS_PATH, "priv/scene_nls").
-define(SCENE_INFO, scene_info).
-define(SCENE_OBJECT_LIST, scene_object_list).
-define(NLS_MAP, nls_map).

-type player() :: {player, Uid :: atom()}.
-type npc_fsm() :: {npc, NpcFsmUuid :: npc_fsm_manager:uuid(), npc_fsm_manager:npc_type(), NpcNameNlsKey :: atom()}.
-type scene_object() :: player() | npc_fsm().

-export_type([npc_fsm/0]).

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
-spec start_link(Request) -> {ok, pid()} | ignore | {error, Reason :: term()} when
    Request :: {init, SceneInfo},
    SceneInfo :: map().
start_link({init, #{id := SceneName} = SceneInfo}) ->
    gen_fsm:start_link({local, SceneName}, ?MODULE, {init, SceneInfo}, []).

%%--------------------------------------------------------------------
%% @doc
%% Enter a scene. When entering a scene, the scene information of
%% this scene is initially response to user and the player scene id
%% is then added to uid map of the scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec enter(SceneName, Uid, Lang, DispatcherPid) -> ok when
    SceneName :: atom(),
    Uid :: atom(),
    Lang :: atom(),
    DispatcherPid :: pid().
enter(SceneName, Uid, Lang, DispatcherPid) ->
    gen_fsm:send_all_state_event(SceneName, {enter, Uid, Lang, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Go target direction. This function first checks whether the scene
%% name of target direction exists, if so, remove player uid from uid
%% list of scene state and returns the target scene name, or else
%% returns {undefined, SceneNlsServerName} but does nothing.
%%
%% @end
%%--------------------------------------------------------------------
-spec go_direction(SceneName, Uid, Lang, DispatcherPid, TargetDirection) -> Result when
    SceneName :: atom(),
    Uid :: atom(),
    Lang :: atom(),
    DispatcherPid :: pid(),
    TargetDirection :: direction:directions(),
    Result :: TargetSceneName | {undefined, SceneNlsServerName},
    TargetSceneName :: atom(),
    SceneNlsServerName :: atom().
go_direction(SceneName, Uid, Lang, DispatcherPid, TargetDirection) ->
    gen_fsm:sync_send_all_state_event(SceneName, {go_direction, Uid, Lang, DispatcherPid, TargetDirection}).

%%--------------------------------------------------------------------
%% @doc
%% Leaves scene by removing player uid from uid map of scene state.
%% This is usually called when player logging out.
%%
%% @end
%%--------------------------------------------------------------------
-spec leave(SceneName, Uid) -> ok when
    SceneName :: atom(),
    Uid :: map().
leave(SceneName, Uid) ->
    gen_fsm:send_all_state_event(SceneName, {leave, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Looks at current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_scene(CurSceneName, Uid, Lang, DispatcherPid) -> ok when
    CurSceneName :: atom(),
    DispatcherPid :: pid(),
    Uid :: atom(),
    Lang :: atom().
look_scene(CurSceneName, Uid, Lang, DispatcherPid) ->
    gen_fsm:send_all_state_event(CurSceneName, {look_scene, Uid, Lang, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Looks at current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_target(CurSceneName, Uid, Lang, DispatcherPid, Target, Sequence) -> ok when
    CurSceneName :: atom(),
    DispatcherPid :: pid(),
    Uid :: atom(),
    Lang :: atom(),
    Target :: look:target(),
    Sequence :: look:sequence().
look_target(CurSceneName, Uid, Lang, DispatcherPid, Target, Sequence) ->
    gen_fsm:send_all_state_event(CurSceneName, {look_target, Uid, Lang, DispatcherPid, Target, Sequence}).

%%--------------------------------------------------------------------
%% @doc
%% Terminate this scene fsm.
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

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
-spec init(Args :: term()) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when
    StateName :: atom(),
    StateData :: map(),
    Reason :: term().
init({init, #{npcs := NpcsSpec, nls_files := NlsFileNames} = SceneInfo}) ->
    error_logger:info_msg("Starting scene:~p~n", [SceneInfo]),

    CommonNlsFilePath = filename:append(?NLS_PATH, ?COMMON_NLS),
    CommonNlsMap = nls_server:read_nls_file(CommonNlsFilePath, #{}),
    NlsMap = lists:foldl(fun load_nls_file/2, CommonNlsMap, string:tokens(NlsFileNames, ",")),

    SceneNpcFsmList = npc_fsm_manager:new_npcs(NpcsSpec),
    {ok, state_name, #{?SCENE_INFO => SceneInfo, ?SCENE_OBJECT_LIST => SceneNpcFsmList, ?NLS_MAP => NlsMap}}.

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
    Event :: term(),
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
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
    Event :: term(),
    From :: {pid(), term()},
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    Reason :: normal | term(),
    Reply :: term(),
    NewState :: map().
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
    {enter, Uid, Lang, DispatcherPid} |
    {leave, Uid} |
    {look_scene, Uid, Lang, DispatcherPid} |
    {look_target, Uid, Lang, DispatcherPid, Target, Sequence},

    DispatcherPid :: pid(),
    Uid :: atom(),
    Lang :: atom(),
    StateName :: atom(),
    StateData :: map(),
    NextStateName :: atom(),
    NewStateData :: map(),
    Reason :: term(),
    Target :: look:target(),
    Sequence :: look:sequence().
handle_event({enter, Uid, Lang, DispatcherPid}, StateName, #{?SCENE_OBJECT_LIST := SceneObjectList} = State) ->
    ok = show_scene(State, Uid, Lang, DispatcherPid),
    {next_state, StateName, State#{?SCENE_OBJECT_LIST := [{player, Uid} | SceneObjectList]}};
handle_event({leave, Uid}, StateName, State) ->
    {next_state, StateName, remove_scene_object({player, Uid}, State)};
handle_event({look_scene, Uid, Lang, DispatcherPid}, StateName, State) ->
    ok = show_scene(State, Uid, Lang, DispatcherPid),
    {next_state, StateName, State};
handle_event({look_target, Uid, Lang, DispatcherPid, Target, Sequence}, StateName, #{?SCENE_OBJECT_LIST := SceneObjectList, ?NLS_MAP := NlsMap} = State) ->
    TargetSceneObject = grab_target_scene_objects(SceneObjectList, Target, Sequence),
    ok = case TargetSceneObject of
             undefined ->
                 nls_server:do_response_content(Lang, NlsMap, [{nls, no_such_target}, <<"\n">>], DispatcherPid);
             {npc, TargetNpcFsmId, _, _} ->
                 ContentList = npc_fsm:being_looked(TargetNpcFsmId, Uid),
                 nls_server:do_response_content(Lang, NlsMap, ContentList, DispatcherPid);
             {player, _TargetPlayerFsmId} ->
                 % player_fsm:get_description(TargetPlayerFsmId, PlayerProfile, NlsServerName, Lang, DispatcherPid)
                 ok % TODO: implement above function after player nickname and id features are done.
         end,
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% Filters scene objects by given target name.
%%
%% @end
%%--------------------------------------------------------------------
-spec grab_target_scene_objects(SceneObjectList, TargetName, Sequence) -> TargetSceneObject when
    SceneObjectList :: [scene_object()],
    TargetName :: atom(),
    Sequence :: look:sequence(),
    TargetSceneObject :: scene_object() | undefined.
grab_target_scene_objects(SceneObjectList, TargetName, Sequence) ->
    grab_target_scene_objects(SceneObjectList, TargetName, Sequence, 1).

%%--------------------------------------------------------------------
%% @doc
%% See parent function grab_target_scene_objects/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec grab_target_scene_objects(SceneObjectList, TargetName, Sequence, Counter) -> TargetSceneObject when
    SceneObjectList :: [scene_object()],
    TargetName :: atom(),
    Sequence :: look:sequence(),
    Counter :: pos_integer(),
    TargetSceneObject :: scene_object() | undefined.
grab_target_scene_objects([{npc, _, TargetName, _} = SceneObject | _], TargetName, Sequence, Sequence) ->
    SceneObject;
grab_target_scene_objects([{player, TargetName} = SceneObject | _], TargetName, Sequence, Sequence) ->
    SceneObject;
grab_target_scene_objects([{npc, _, TargetName, _} | Tail], TargetName, Sequence, Counter) ->
    grab_target_scene_objects(Tail, TargetName, Sequence, Counter + 1);
grab_target_scene_objects([{player, TargetName} | Tail], TargetName, Sequence, Counter) ->
    grab_target_scene_objects(Tail, TargetName, Sequence, Counter + 1);
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
-spec remove_scene_object(SceneObject, State) -> UpdatedState when
    SceneObject :: scene_object(),
    State :: map(),
    UpdatedState :: map().
remove_scene_object(SceneObject, #{?SCENE_OBJECT_LIST := SceneObjectList} = State) ->
    State#{?SCENE_OBJECT_LIST := lists:delete(SceneObject, SceneObjectList)}.

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

    Event :: {go_direction, Uid, Lang, DispatcherPid, TargetDirection},
    Uid :: atom(),
    Lang :: atom(),
    DispatcherPid :: pid(),
    TargetDirection :: direction:directions(),
    From :: {pid(), Tag :: term()},
    StateName :: atom(),
    StateData :: term(),
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
    Reason :: term().
handle_sync_event({go_direction, Uid, Lang, DispatcherPid, TargetDirection}, _From, StateName, #{?SCENE_INFO := #{exits := ExitsMap}, ?NLS_MAP := NlsMap} = State) ->
    {TargetSceneName, UpdatedState} =
        case maps:get(TargetDirection, ExitsMap, undefined) of
            undefined ->
                nls_server:do_response_content(Lang, NlsMap, [{nls, invalid_exit}], DispatcherPid),
                {undefined, State};
            SceneName ->
                {SceneName, remove_scene_object({player, Uid}, State)}
        end,

    {reply, TargetSceneName, StateName, UpdatedState}.

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
    Info :: term(),
    StateName :: atom(),
    StateData :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
    Reason :: normal | term().
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
-spec terminate(Reason, StateName, StateData) -> term() when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    StateName :: atom(),
    StateData :: term().
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
    OldVsn :: term() | {down, term()},
    StateName :: atom(),
    StateData :: map(),
    Extra :: term(),
    NextStateName :: atom(),
    NewStateData :: map().
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
    PDict :: [{Key :: term(), Value :: term()}],
    State :: term(),
    Status :: term().
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
    ExitsMap :: map(),
    ExitNls :: [{nls, atom()} | binary()].
gen_exits_desc(ExitsMap) ->
    [[{nls, ExitKey}, <<"\n">>] || ExitKey <- maps:keys(ExitsMap)].

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene player and npc name list without the caller player.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_characters_name_list(SceneObjectList, CallerUid) -> [term()] when
    SceneObjectList :: [scene_object()],
    CallerUid :: atom().
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
    CallerUid :: atom(),
    AccSceneObjectNameList :: [term()],
    SceneObjectNameList :: AccSceneObjectNameList.
gen_character_name([], _, AccList) ->
    AccList;
gen_character_name([{npc, _, NpcType, NpcNameNlsKey} | Tail], CallerUid, AccSceneObjectNameList) ->
    gen_character_name(Tail, CallerUid, [{nls, NpcNameNlsKey}, <<" (">>, atom_to_binary(NpcType, utf8), <<")">>, <<"\n">>] ++ AccSceneObjectNameList);
gen_character_name([{player, CallerUid} | Tail], CallerUid, AccCharactersNameList) ->
    gen_character_name(Tail, CallerUid, AccCharactersNameList);
gen_character_name([{player, Uid} | Tail], CallerUid, AccSceneObjectNameList) ->
    gen_character_name(Tail, CallerUid, [atom_to_binary(Uid, utf8), <<"\n">>] ++ AccSceneObjectNameList).

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene info including scene title, scene description,
%% scene player name list, and exits.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_scene(State, Uid, Lang, DispatcherPid) -> ok when
    DispatcherPid :: pid(),
    State :: map(),
    Uid :: atom(),
    Lang :: atom().
show_scene(#{?SCENE_INFO := #{exits := ExitsMap, title := SceneTitle, desc := SceneDesc}, ?NLS_MAP := NlsMap, ?SCENE_OBJECT_LIST :=  SceneObjectList}, Uid, Lang, DispatcherPid) ->
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
    nls_server:do_response_content(Lang, NlsMap, ContentList, DispatcherPid).

%%--------------------------------------------------------------------
%% @doc
%% Loads nls file. This function is called by lists:foldl/3 or lists:foldr/3.
%% @see lists:foldl/3, lists:foldr/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_nls_file(NlsFileName, AccNlsMap) -> NlsMap when
    NlsFileName :: file:filename_all(),
    AccNlsMap :: map(),
    NlsMap :: AccNlsMap.
load_nls_file(NlsFileName, AccNlsMap) ->
    nls_server:read_nls_file(filename:join(?SCENE_NLS_PATH, NlsFileName) ++ ?NLS_EXTENSION, AccNlsMap).