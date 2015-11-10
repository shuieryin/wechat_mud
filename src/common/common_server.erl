%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Common gen_server. This server maintains systemic states or variables.
%%%
%%% @end
%%% Created : 03. Sep 2015 3:50 PM
%%%-------------------------------------------------------------------
-module(common_server).
-author("Shuieryin").

-behaviour(gen_server).

%% API
-export([start_link/0,
    is_wechat_debug/0,
    turn_on_wechat_debug/0,
    turn_off_wechat_debug/0,
    get_runtime_data/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-define(SERVER, ?MODULE).
-define(R_COMMON_CONFIG, common_config).
-define(DEFAULT_WECHAT_DEBUG_MODE, true).
-define(RUNTIME_DATAS, runtime_datas).

-type state() :: #{?R_COMMON_CONFIG => #{is_wechat_debug => boolean()}, ?RUNTIME_DATAS => csv_to_object:csv_to_object()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts server by setting module name as server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Checks if wechat debug mode is on.
%%
%% If wechat debug mode is on, the signature validation will be skipped
%% by requests sent from wechat debug tool (http://mp.weixin.qq.com/debug).
%%
%% @end
%%--------------------------------------------------------------------
-spec is_wechat_debug() -> boolean().
is_wechat_debug() ->
    gen_server:call(?MODULE, {is_wechat_debug}).

%%--------------------------------------------------------------------
%% @doc
%% Turns on wechat debug mode.
%%
%% If wechat debug mode is on, the signature validation will be skipped
%% by requests sent from wechat debug tool (http://mp.weixin.qq.com/debug).
%%
%% @end
%%--------------------------------------------------------------------
-spec turn_on_wechat_debug() -> boolean().
turn_on_wechat_debug() ->
    gen_server:call(?MODULE, {set_wechat_debug, true}).

%%--------------------------------------------------------------------
%% @doc
%% Turns off wechat debug mode.
%%
%% If wechat debug mode is on, the signature validation will be skipped
%% by requests sent from wechat debug tool (http://mp.weixin.qq.com/debug).
%%
%% @end
%%--------------------------------------------------------------------
-spec turn_off_wechat_debug() -> boolean().
turn_off_wechat_debug() ->
    gen_server:call(?MODULE, {set_wechat_debug, false}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves runtime data by phase list.
%% For example retriving specific:
%%      csv file:                   [csv_file_name]
%%      row of csv file:            [csv_file_name | row_id]
%%      field of row of csv file:   [csv_file_name | row_id | field_name]
%%
%% The element types of each phase has to be map type except csv_file_name
%% and the last phase name. It returns undefined once field not found when
%% traversing phase list.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_runtime_data(Phases) -> RuntimeDataMap when
    Phases :: [csv_to_object:key()],
    RuntimeDataMap :: csv_to_object:csv_data().
get_runtime_data(Phases) ->
    gen_server:call(?MODULE, {get_runtime_data, Phases}).

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
-spec init([]) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    State :: state(),
    Reason :: term(). % generic term
init([]) ->
    io:format("~p starting...", [?MODULE]),

    CommonConfig =
        case redis_client_server:get(?R_COMMON_CONFIG) of
            undefined ->
                NewConfig = #{},
                redis_client_server:set(?R_COMMON_CONFIG, NewConfig, true),
                NewConfig;
            ReturnValue ->
                ReturnValue
        end,

    RuntimeDatas = csv_to_object:traverse_files("priv/runtime"),
    State = #{?R_COMMON_CONFIG => CommonConfig, ?RUNTIME_DATAS => RuntimeDatas},

    io:format("done~n"),
    {ok, State}.

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

    Request ::
    {is_wechat_debug} |
    {set_wechat_debug, IsOn :: boolean()} |
    {get_runtime_data, Phases :: [csv_to_object:key()]},
    Reply :: IsWechatDebug | IsOn | TargetRuntimeData,

    IsWechatDebug :: boolean(),
    IsOn :: boolean(),
    TargetRuntimeData :: csv_to_object:csv_data(),

    From :: {pid(), Tag :: term()}, % generic term
    State :: state(),
    NewState :: State,
    Reason :: term(). % generic term
handle_call({is_wechat_debug}, _From, #{?R_COMMON_CONFIG := CommonConfigs} = State) ->
    IsWechatDebug = maps:get(is_wechat_debug, CommonConfigs, ?DEFAULT_WECHAT_DEBUG_MODE),
    {reply, IsWechatDebug, State};
handle_call({set_wechat_debug, IsOn}, _From, #{?R_COMMON_CONFIG := CommonConfigs} = State) ->
    {reply, IsOn, State#{?R_COMMON_CONFIG := CommonConfigs#{is_wechat_debug => IsOn}}};
handle_call({get_runtime_data, Phases}, _From, #{?RUNTIME_DATAS := RuntimeDatasMap} = State) ->
    TargetRuntimeData = get_runtime_data(Phases, RuntimeDatasMap),
    {reply, TargetRuntimeData, State}.


%%--------------------------------------------------------------------
%% @doc
%% Get runtime data by phases, for details see get_runtime_data/1
%%
%% @end
%%--------------------------------------------------------------------
-spec get_runtime_data(Phases, RuntimeDatasMap) -> TargetRuntimeData when
    Phases :: [csv_to_object:key()],
    RuntimeDatasMap :: csv_to_object:csv_to_object(),
    TargetRuntimeData :: term(). % generic term
get_runtime_data([Phase | []], RuntimeDatasMap) ->
    case maps:get(Phase, RuntimeDatasMap, undefined) of
        undefined ->
            undefined;
        TargetRuntimeData ->
            TargetRuntimeData
    end;
get_runtime_data([Phase | Tail], RuntimeDatasMap) ->
    case maps:get(Phase, RuntimeDatasMap, undefined) of
        undefined ->
            undefined;
        DeeperMap ->
            get_runtime_data(Tail, DeeperMap)
    end.

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

    Request :: term(), % generic term
    State :: state(),
    NewState :: State,
    Reason :: term(). % generic term
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
    State :: state(),
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
    State :: state().
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
    State :: state(),
    Extra :: term(), % generic term
    NewState :: State,
    Reason :: term(). % generic term
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    State :: state(),
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================
