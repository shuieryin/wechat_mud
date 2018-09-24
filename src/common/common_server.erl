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
-export([
    start_link/0,
    is_wechat_debug/0,
    turn_on_wechat_debug/0,
    turn_off_wechat_debug/0,
    runtime_data/1,
    runtime_data/2,
    runtime_datas/1,
    random_npc/0
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
-define(DEFAULT_WECHAT_DEBUG_MODE, true).
-define(EMPTY_CONTENT, <<>>).

-include("../data_type/npc_profile.hrl").

-record(common_config, {
    is_wechat_debug = false :: boolean()
}).

-record(state, {
    common_config :: #common_config{} | undefined,
    runtime_datas :: csv_to_object:csv_object()
}).

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
    gen_server:call(?SERVER, is_wechat_debug).

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
    gen_server:call(?SERVER, {set_wechat_debug, true}).

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
    gen_server:call(?SERVER, {set_wechat_debug, false}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves runtime data by file name.
%%
%% For example retriving specific:
%%
%%      csv file:                   [csv_file_name]
%%
%% @end
%%--------------------------------------------------------------------
-spec runtime_data(FileName) -> RuntimeData when
    FileName :: csv_to_object:key(),
    RuntimeData :: csv_to_object:csv_data().
runtime_data(DataName) ->
    RuntimeDatas = gen_server:call(?SERVER, runtime_datas),
    grab_runtime_data([DataName], RuntimeDatas).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves runtime data by phase list.
%%
%% For example retriving specific:
%%
%%      csv file:                   [csv_file_name]
%%
%%      row of csv file:            [csv_file_name | row_id]
%%
%%      field of row of csv file:   [csv_file_name | row_id | field_name]
%%
%%
%% The element types of each phase has to be map type except csv_file_name
%% and the last phase name. It returns undefined once field not found when
%% traversing phase list.
%%
%% @end
%%--------------------------------------------------------------------
-spec runtime_data(DataName, RecordName) -> RuntimeRecord when
    DataName :: csv_to_object:key(),
    RecordName :: term(), % generic term
    RuntimeRecord :: csv_to_object:csv_row_data().
runtime_data(DataName, RecordName) ->
    RuntimeDatas = gen_server:call(?SERVER, runtime_datas),
    grab_runtime_data([DataName, RecordName], RuntimeDatas).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves runtime data by file name.
%%
%% For example retriving specific:
%%
%%      csv file:                   [csv_file_name]
%%
%% @end
%%--------------------------------------------------------------------
-spec runtime_datas(TargetDataStruct) -> RuntimeDatas when
    TargetDataStruct :: [csv_to_object:csv_data_struct()],
    RuntimeDatas :: csv_to_object:csv_object().
runtime_datas(TargetDataStruct) ->
    State = gen_server:call(?SERVER, common_state),
    grab_runtime_datas(State, TargetDataStruct).


%%--------------------------------------------------------------------
%% @doc
%% Randomly select an npc profile.
%%
%% @end
%%--------------------------------------------------------------------
-spec random_npc() -> NpcProfile when
    NpcProfile :: #npc_profile{}.
random_npc() ->
    #{
        npc_profile := NpcsRuntimeDataMap
    } = gen_server:call(?SERVER, runtime_datas),
    RandomKey = elib:random_from_list(maps:keys(NpcsRuntimeDataMap)),
    #{RandomKey := RandomNpc} = NpcsRuntimeDataMap,
    RandomNpc.

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

    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    io:format("~p starting...", [?MODULE]),

    CommonConfig =
        case redis_client_server:get(common_config) of
            undefined ->
                NewConfig = #common_config{is_wechat_debug = ?DEFAULT_WECHAT_DEBUG_MODE},
                true = redis_client_server:set(common_config, NewConfig, true),
                NewConfig;
            Config ->
                Config
        end,

    RuntimeFilePath = filename:join(code:priv_dir(elib:app_name()), ?MODULE_STRING),
    {ok, FileNameList} = file:list_dir(RuntimeFilePath),
    FilePathList = [filename:join(RuntimeFilePath, FileName) || FileName <- FileNameList],
    {RuntimeDatas, _ChangedRuntimeDatas, _DeletedFilesStruct} = csv_to_object:traverse_files(FilePathList, #{}, #{}),
    State = #state{
        common_config = CommonConfig,
        runtime_datas = RuntimeDatas
    },

    BasePath = filename:join([filename:dirname(code:lib_dir(elib:app_name())), <<"../misc">>]),
    {ok, RawKey} = file:read_file(filename:join([BasePath, <<"EncodingAESKey">>])),
    EncodingAESKey = re:replace(RawKey, "\n", "", [global, {return, binary}]),
    DecodedAESKey = base64:mime_decode(EncodingAESKey),

    {ok, RawWechatToken} = file:read_file(filename:join([BasePath, <<"WechatToken">>])),
    WechatToken = re:replace(RawWechatToken, "\n", "", [global, {return, binary}]),

    {ok, RawAppId} = file:read_file(filename:join([BasePath, <<"AppId">>])),
    AppId = re:replace(RawAppId, "\n", "", [global, {return, binary}]),

    TableId = ets:new(misc_table, [set, protected, named_table]),
    ets:insert(TableId, {DecodedAESKey, WechatToken, AppId}),

    io:format("started~n~n"),
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
    is_wechat_debug |
    {set_wechat_debug, IsWechatDebug} |
    runtime_datas |
    common_state,

    Reply ::
    IsWechatDebug |
    RuntimeDatas,

    IsWechatDebug :: boolean(),
    RuntimeDatas :: csv_to_object:csv_object(),

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(
    is_wechat_debug,
    _From,
    #state{
        common_config = CommonConfig
    } = State
) ->
    Reply = case CommonConfig of
                undefined ->
                    false;
                #common_config{
                    is_wechat_debug = IsWechatDebug
                } ->
                    IsWechatDebug
            end,
    {reply, Reply, State};
handle_call(
    {set_wechat_debug, IsWechatDebug},
    _From,
    #state{
        common_config = CommonConfigs
    } = State
) ->
    {reply, IsWechatDebug, State#state{
        common_config = CommonConfigs#common_config{
            is_wechat_debug = IsWechatDebug
        }
    }};
handle_call(runtime_datas, _From, #state{
    runtime_datas = RuntimeDatas
} = State) ->
    {reply, RuntimeDatas, State};
handle_call(common_state, _From, State) ->
    {reply, State, State}.

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

    Request :: stop,

    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast(stop, State) ->
    {stop, normal, State}.

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
code_change(
    _OldVsn,
    #state{
        runtime_datas = OldRuntimeDatas
    } = State,
    Extra
) ->
    try
        case Extra of
            {_OldVer, _NewVsn, PrivChangedFiles} ->
                case csv_to_object:convert_priv_paths(PrivChangedFiles) of
                    no_change ->
                        {ok, State};
                    {ModifiedFilePaths, AddedFilePaths, DeletedFileNames} ->
                        RemoveNotUsedData = maps:without(DeletedFileNames, OldRuntimeDatas),

                        ReloadFilePaths = AddedFilePaths ++ ModifiedFilePaths, % number of add files is usually less than modified files
                        {NewRuntimeDatas, ChangedFilesMap, DeletedFilesStruct} = csv_to_object:traverse_files(ReloadFilePaths, RemoveNotUsedData, #{}),
                        error_logger:info_msg("~p~n============changed data~n~tp~n", [?MODULE_STRING, ChangedFilesMap]),

                        ChangedFilesStruct = maps:fold(
                            fun(FileName, ValuesMap, AccChangedList) ->
                                [{FileName, maps:keys(ValuesMap)} | AccChangedList]
                            end, [], ChangedFilesMap),

                        ok = gb_sets:fold(
                            fun(PlayerUid, ok) ->
                                player_statem:pending_update_runtime_data(PlayerUid, {ChangedFilesStruct, DeletedFilesStruct ++ DeletedFileNames})
                            end, ok, login_server:logged_in_player_uids()),

                        {ok, State#state{
                            runtime_datas = NewRuntimeDatas
                        }}
                end;
            _NoChange ->
                {ok, State}
        end
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
-spec grab_runtime_data(Phases, RuntimeDatasMap) -> TargetRuntimeData when
    Phases :: [csv_to_object:key()],
    RuntimeDatasMap :: csv_to_object:csv_object(),
    TargetRuntimeData :: term(). % generic term
grab_runtime_data([Phase | []], RuntimeDatasMap) ->
    case maps:get(Phase, RuntimeDatasMap, undefined) of
        undefined ->
            undefined;
        TargetRuntimeData ->
            TargetRuntimeData
    end;
grab_runtime_data([Phase | Tail], RuntimeDatasMap) ->
    case maps:get(Phase, RuntimeDatasMap, undefined) of
        undefined ->
            undefined;
        DeeperMap ->
            grab_runtime_data(Tail, DeeperMap)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves runtime datas by data struct.
%%
%% @end
%%--------------------------------------------------------------------
-spec grab_runtime_datas(State, DataStruct) -> TargetRuntimeDataMap when
    State :: #state{},
    DataStruct :: [csv_to_object:csv_data_struct()],
    TargetRuntimeDataMap :: csv_to_object:csv_object().
grab_runtime_datas(#state{
    runtime_datas = RuntimeDatasMap
} = State, DataStruct) ->
    lists:foldl(
        fun
            ({DataKey, RecordKeys}, AccTargetRuntimeDataMap) ->
                DataMap = maps:get(DataKey, RuntimeDatasMap),
                AccTargetRuntimeDataMap#{
                    DataKey => maps:with(RecordKeys, DataMap)
                };
            ({DataKey, RecordKeys, DependencyDataStructFunc}, AccTargetRuntimeDataMap) ->
                TargetDataMap = maps:with(RecordKeys, maps:get(DataKey, RuntimeDatasMap)),
                DependencyDataStruct = DependencyDataStructFunc(TargetDataMap),
                DependencyDataMap = grab_runtime_datas(State, DependencyDataStruct),
                maps:merge(AccTargetRuntimeDataMap#{
                    DataKey => TargetDataMap
                }, DependencyDataMap);
            (DataKey, AccTargetRuntimeDataMap) ->
                AccTargetRuntimeDataMap#{
                    DataKey => maps:get(DataKey, RuntimeDatasMap)
                }
        end, #{}, DataStruct).