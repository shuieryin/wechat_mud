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
    enter/3,
    leave/2,
    go_direction/3,
    look/3]).

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

-define(PLAYERS_MAP, players_map).

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
    Request :: {init, ValuesMap},
    ValuesMap :: map().
start_link({init, #{id := SceneName} = ValuesMap}) ->
    gen_fsm:start_link({local, SceneName}, ?MODULE, [{init, ValuesMap}], []).

%%--------------------------------------------------------------------
%% @doc
%% Enter a scene. When entering a scene, the scene information of
%% this scene is initially response to user and the player scene id
%% is then added to uid map of the scene.
%%
%% @end
%%--------------------------------------------------------------------
-spec enter(SceneName, SimplePlayerProfile, DispatcherPid) -> ok when
    SceneName :: atom(),
    SimplePlayerProfile :: map(),
    DispatcherPid :: pid().
enter(SceneName, SimplePlayerProfile, DispatcherPid) ->
    gen_fsm:send_all_state_event(SceneName, {enter, SimplePlayerProfile, DispatcherPid}).

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
    SceneName :: atom(),
    Uid :: atom(),
    TargetDirection :: direction:directions(),
    Result :: TargetSceneName | {undefined, SceneNlsServerName},
    TargetSceneName :: atom(),
    SceneNlsServerName :: atom().
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
    SceneName :: atom(),
    Uid :: map().
leave(SceneName, Uid) ->
    gen_fsm:send_all_state_event(SceneName, {leave, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Looks current scene by response current scene info to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec look(CurSceneName, DispatcherPid, SimplePlayerProfile) -> ok when
    CurSceneName :: atom(),
    DispatcherPid :: pid(),
    SimplePlayerProfile :: map().
look(CurSceneName, DispatcherPid, SimplePlayerProfile) ->
    gen_fsm:send_all_state_event(CurSceneName, {look, DispatcherPid, SimplePlayerProfile}).

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
init([{init, SceneInfo}]) ->
    error_logger:info_msg("Starting scene:~p~n", [SceneInfo]),
    {ok, state_name, #{scene_info => SceneInfo, ?PLAYERS_MAP => #{}}}.

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

    Event :: {enter, SimplePlayerProfile, DispatcherPid} | {leave, Uid} | {look, DispatcherPid},
    SimplePlayerProfile :: map(),
    DispatcherPid :: pid(),
    Uid :: atom(),
    StateName :: atom(),
    StateData :: map(),
    NextStateName :: atom(),
    NewStateData :: map(),
    Reason :: term().
handle_event({enter, #{uid := Uid} = SimplePlayerProfile, DispatcherPid}, StateName, #{?PLAYERS_MAP := PlayersMap} = State) ->
    show_scene(DispatcherPid, State, SimplePlayerProfile),
    {next_state, StateName, State#{?PLAYERS_MAP := PlayersMap#{Uid => SimplePlayerProfile}}};
handle_event({leave, Uid}, StateName, #{?PLAYERS_MAP := PlayersMap} = State) ->
    {next_state, StateName, State#{?PLAYERS_MAP := maps:remove(Uid, PlayersMap)}};
handle_event({look, DispatcherPid, SimplePlayerProfile}, StateName, State) ->
    show_scene(DispatcherPid, State, SimplePlayerProfile),
    {next_state, StateName, State}.

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

    Event :: {go_direction, Uid, TargetDirection},
    Uid :: atom(),
    TargetDirection :: direction:directions(),
    From :: {pid(), Tag :: term()},
    StateName :: atom(),
    StateData :: term(),
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
    Reason :: term().
handle_sync_event({go_direction, Uid, TargetDirection}, _From, StateName, #{scene_info := #{exits := ExitsMap, nls_server := SceneNlsServerName}, ?PLAYERS_MAP := PlayersMap} = State) ->
    {TargetSceneName, UpdatedState} =
        case maps:get(TargetDirection, ExitsMap, undefined) of
            undefined ->
                {{undefined, SceneNlsServerName}, State};
            SceneName ->
                {SceneName, State#{?PLAYERS_MAP := maps:remove(Uid, PlayersMap)}}
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
%% Prepares scene player name list.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_players_name_list(PlayersMap) -> [term()] when
    PlayersMap :: map().
gen_players_name_list(PlayersMap) ->
    UidList = maps:keys(PlayersMap),
    case length(UidList) of
        0 ->
            [];
        _ ->
            [[[atom_to_binary(Uid, utf8), <<"\n">>] || Uid <- maps:keys(PlayersMap)], <<"\n">>]
    end.

%%--------------------------------------------------------------------
%% @doc
%% Prepares scene info including scene title, scene description,
%% scene player name list, and exits.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_scene(DispatcherPid, State, SimplePlayerProfile) -> ok when
    DispatcherPid :: pid(),
    State :: map(),
    SimplePlayerProfile :: map().
show_scene(DispatcherPid, #{scene_info := #{exits := ExitsMap, title := SceneTitle, desc := SceneDesc, nls_server := NlsServerName}, ?PLAYERS_MAP := PlayersMap}, #{uid := Uid, lang := Lang}) ->
    WithoutUidPlayersMap = maps:remove(Uid, PlayersMap),
    nls_server:response_content(NlsServerName,
        lists:flatten([
            {nls, SceneTitle},
            <<"\n\n">>,
            {nls, SceneDesc},
            <<"\n\n">>,
            gen_players_name_list(WithoutUidPlayersMap),
            {nls, obvious_exits},
            <<"\n">>,
            gen_exits_desc(ExitsMap)
        ]),
        Lang, DispatcherPid).