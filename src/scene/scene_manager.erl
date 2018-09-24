%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2016 9:16 PM
%%%-------------------------------------------------------------------
-module(scene_manager).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    scene_specs_map/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).

-define(SERVER, ?MODULE).

-type scene_specs_map() :: #{scene_statem:scene_name() => scene_statem_sup:scene_child()}.

-export_type([
    scene_specs_map/0
]).

-record(state, {
    scene_specs_map :: scene_specs_map()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve scenes map.
%%
%% @end
%%--------------------------------------------------------------------
-spec scene_specs_map() -> scene_specs_map().
scene_specs_map() ->
    gen_server:call(?MODULE, scene_specs_map).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: term(),
    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    {SceneSpecsMap, _ChangedSceneSpecsMap} = load_scene_specs(#{}),
    {ok, #state{
        scene_specs_map = SceneSpecsMap
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState} |
    {reply, Reply, NewState, timeout() | hibernate} |
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, Reply, NewState} |
    {stop, Reason, NewState} when

    Request :: scene_specs_map,
    Reply :: scene_specs_map(),

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(scene_specs_map, _From, #state{
    scene_specs_map = SceneSpecsMap
} = State) ->
    {reply, SceneSpecsMap, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Request :: term() | stop, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info | timeout(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: term(), % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> ok when
    Reason :: (normal | shutdown | {shutdown, term()} | term()), % generic term
    State :: #state{}.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
    {ok, NewState} |
    {error, Reason} when

    OldVsn :: term() | {down, term()}, % generic term
    State :: #state{},
    Extra :: term(), % generic term
    NewState :: State,
    Reason :: term(). % generic term
code_change(_OldVsn, State, _Extra) ->
    try
        #state{
            scene_specs_map = OldSceneSpecsMap
        } = State,
        {SceneSpecsMap, ChangedSceneSpecsMap} = load_scene_specs(OldSceneSpecsMap),
        error_logger:info_msg("~p~n============changed scenes~n~tp~n", [?MODULE_STRING, ChangedSceneSpecsMap]),
        ok = maps:fold(
            fun(SceneName, {_SceneName, {scene_fsm, start_link, [SceneInfo]}, _RestartTime, _ShutdownTime, _WorkerType, [scene_fsm]}, ok) ->
                scene_statem:update_scene_info(SceneName, SceneInfo)
            end, ok, ChangedSceneSpecsMap),
        {ok, State#state{
            scene_specs_map = SceneSpecsMap
        }}
    catch
        Type:Reason:Stacktrace ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, Stacktrace]),
            {ok, State}
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
    State :: #state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Load scene specs from csv file and generate child specs map.
%% This function is usually called by init/1 and code_change/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_scene_specs(ExistingSceneSpecsMap) -> {SceneSpecsMap, ChangedSceneSpecsMap} when
    SceneSpecsMap :: scene_specs_map(),
    ExistingSceneSpecsMap :: SceneSpecsMap,
    ChangedSceneSpecsMap :: SceneSpecsMap.
load_scene_specs(ExistingSceneSpecsMap) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChildFun =
        fun(SceneValues) ->
            scene_statem:scene_child_spec(SceneValues, Restart, Shutdown, Type)
        end,

    SceneNlsPath = filename:join(code:priv_dir(elib:app_name()), ?MODULE_STRING),
    {ok, FileNameList} = file:list_dir(SceneNlsPath),
    FilePathList = [filename:join(SceneNlsPath, FileName) || FileName <- FileNameList],
    csv_to_object:traverse_merge_files(FilePathList, #{}, ExistingSceneSpecsMap, ChildFun).