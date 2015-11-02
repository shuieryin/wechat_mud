%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Login gen_server. This server supports overall user login, logout,
%%% and register services as well as maintains logged in and registered uids.
%%%
%%% @end
%%% Created : 25. Aug 2015 9:11 PM
%%%-------------------------------------------------------------------
-module(login_server).
-author("Shuieryin").

-behaviour(gen_server).

%% API
-export([start_link/0,
    is_uid_registered/1,
    is_in_registration/1,
    register_uid/2,
    registration_done/2,
    delete_player/1,
    login/2,
    is_uid_logged_in/1,
    logout/2]).

-define(R_REGISTERED_UIDS_SET, registered_uids_set).
-define(REGISTERING_UIDS_SET, registration_uids_set).
-define(LOGGED_IN_UIDS_SET, logged_in_uids_set).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts server by setting module name as server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid} |
    ignore |
    {error, Reason} when

    Pid :: pid(),
    Reason :: term().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid has been registered.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_uid_registered(Uid) -> boolean() when
    Uid :: atom().
is_uid_registered(Uid) ->
    gen_server:call(?MODULE, {is_uid_registered, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid is in registration procedure.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_in_registration(Uid) -> boolean() when
    Uid :: atom().
is_in_registration(Uid) ->
    gen_server:call(?MODULE, {is_in_registration, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid is in registration procedure.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_uid(DispatcherPid, Uid) -> ok when
    Uid :: atom(),
    DispatcherPid :: pid().
register_uid(DispatcherPid, Uid) ->
    gen_server:cast(?MODULE, {register_uid, DispatcherPid, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% This function is only called by regsiter_fsm when registration
%% procedure is done. It adds uid to registered uid set, save to
%% redis immediately, and log user in.
%%
%% @end
%%--------------------------------------------------------------------
-spec registration_done(State, DispatcherPid) -> ok when
    State :: map(),
    DispatcherPid :: pid().
registration_done(State, DispatcherPid) ->
    gen_server:cast(?MODULE, {registration_done, State, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% This function deletes player in all states including redis. It logs
%% out player, delete player states from server state, and updates redis.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_player(Uid) -> ok when
    Uid :: atom().
delete_player(Uid) ->
    gen_server:call(?MODULE, {delete_user, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Logs user in by creating its own player_fsm process.
%%
%% @end
%%--------------------------------------------------------------------
-spec login(DispatcherPid, Uid) -> ok when
    DispatcherPid :: pid(),
    Uid :: atom().
login(DispatcherPid, Uid) ->
    gen_server:cast(?MODULE, {login, DispatcherPid, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid has logged in.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_uid_logged_in(Uid) -> boolean() when
    Uid :: atom().
is_uid_logged_in(Uid) ->
    gen_server:call(?MODULE, {is_uid_logged_in, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Logs user out by destroying its player_fsm process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(DispatcherPid, Uid) -> ok when
    DispatcherPid :: pid(),
    Uid :: atom().
logout(DispatcherPid, Uid) ->
    gen_server:cast(?MODULE, {logout, DispatcherPid, Uid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server by retrieves registered uid set from redis
%% and cache to a gb_set, and creates new gb_sets for registering uids
%% and logged in uids for caching.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: term(),
    State :: map(),
    Reason :: term().
init([]) ->
    io:format("~p starting~n", [?MODULE]),
    RegisteredUidsSet =
        case redis_client_server:get(?R_REGISTERED_UIDS_SET) of
            undefined ->
                NewRegisteredUidsSet = gb_sets:new(),
                redis_client_server:set(?R_REGISTERED_UIDS_SET, NewRegisteredUidsSet, true),
                NewRegisteredUidsSet;
            UidList ->
                error_logger:info_msg("Registered uid list:~p~n", [UidList]),
                UidList
        end,
    {ok, #{?REGISTERING_UIDS_SET => gb_sets:new(), ?R_REGISTERED_UIDS_SET => RegisteredUidsSet, ?LOGGED_IN_UIDS_SET => gb_sets:new()}}.

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

    Request :: {is_uid_registered | is_in_registration | delete_user, Uid},
    Uid :: atom(),
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({is_uid_registered, Uid}, _From, #{?R_REGISTERED_UIDS_SET := RegisteredUidsSet} = State) ->
    {reply, gb_sets:is_element(Uid, RegisteredUidsSet), State};
handle_call({is_in_registration, Uid}, _From, #{?REGISTERING_UIDS_SET := RegisteringUidsSet} = State) ->
    Result = gb_sets:is_element(Uid, RegisteringUidsSet),
    {reply, Result, State};
handle_call({delete_user, Uid}, _From, #{?R_REGISTERED_UIDS_SET := RegisteredUidsSet} = State) ->
    LoggedOutState = logout(internal, Uid, State),
    ok = common_api:until_process_terminated(Uid, 20),
    UpdatedRegisteredUidsSet = gb_sets:delete(Uid, RegisteredUidsSet),

    redis_client_server:async_del([Uid], false),
    redis_client_server:async_set(?R_REGISTERED_UIDS_SET, UpdatedRegisteredUidsSet, true),

    {reply, ok, LoggedOutState#{?R_REGISTERED_UIDS_SET := UpdatedRegisteredUidsSet}};
handle_call({is_uid_logged_in, Uid}, _From, #{?LOGGED_IN_UIDS_SET := LoggedUidsSet} = State) ->
    {reply, gb_sets:is_element(Uid, LoggedUidsSet), State}.

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

    Request :: {registration_done, PlayerProfile, DispatcherPid} | {register_uid, DispatcherPid, Uid} | {login, DispatcherPid, Uid},
    DispatcherPid :: pid(),
    Uid :: atom(),
    PlayerProfile :: command_dispatcher:uid_profile(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_cast({registration_done, #{uid := Uid} = PlayerProfile, DispatcherPid}, #{?REGISTERING_UIDS_SET := RegisteringUidsSet, ?R_REGISTERED_UIDS_SET := RegisteredUidsSet} = State) ->
    UpdatedRegisteredUidsSet = gb_sets:add(Uid, RegisteredUidsSet),

    redis_client_server:async_set(Uid, PlayerProfile, false),
    redis_client_server:async_set(?R_REGISTERED_UIDS_SET, UpdatedRegisteredUidsSet, true),

    UpdatedState = State#{?R_REGISTERED_UIDS_SET := UpdatedRegisteredUidsSet, ?REGISTERING_UIDS_SET => gb_sets:del_element(Uid, RegisteringUidsSet)},

    login(DispatcherPid, Uid),
    {noreply, UpdatedState};
handle_cast({register_uid, DispatcherPid, Uid}, State) ->
    UpdatedState =
        case supervisor:start_child(register_fsm_sup, [DispatcherPid, Uid]) of
            {ok, _} ->
                error_logger:info_msg("Started register fsm successfully.~nUid:~p~n", [Uid]),
                RegisteringUidsSet = maps:get(?REGISTERING_UIDS_SET, State),
                State#{?REGISTERING_UIDS_SET := gb_sets:add(Uid, RegisteringUidsSet)};
            {'error', Reason} ->
                error_logger:error_msg("Failed to start register fsm.~nUid:~p~nReason:~p~n", [Uid, Reason]),
                State
        end,
    {noreply, UpdatedState};
handle_cast({login, DispatcherPid, Uid}, #{?LOGGED_IN_UIDS_SET := LoggedInUidsSet} = State) ->
    UpdatedLoggedInUidsSet =
        case gb_sets:is_element(Uid, LoggedInUidsSet) of
            false ->
                #{scene := CurSceneName} = PlayerProfile = redis_client_server:get(Uid),
                supervisor:start_child(player_fsm_sup, [PlayerProfile]),
                scene_fsm:enter(CurSceneName, PlayerProfile, DispatcherPid),
                gb_sets:add(Uid, LoggedInUidsSet);
            _ ->
                player_fsm:response_content(Uid, login, [{nls, already_login}], DispatcherPid),
                LoggedInUidsSet
        end,

    {noreply, State#{?LOGGED_IN_UIDS_SET := UpdatedLoggedInUidsSet}};
handle_cast({logout, DispatcherPid, Uid}, State) ->
    UpdatedState = logout(internal, Uid, State),
    player_fsm:response_content(Uid, logout, [{nls, already_logout}], DispatcherPid),
    {noreply, UpdatedState}.

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
-spec handle_info(Info | term(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: timeout(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
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
-spec terminate(Reason, State) -> term() when
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: map().
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

    OldVsn :: term() | {down, term()},
    State :: map(),
    Extra :: term(),
    NewState :: map(),
    Reason :: term().
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
    PDict :: [{Key :: term(), Value :: term()}],
    State :: term(),
    Status :: term().
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called by "delete_user" and "logout" from
%% handle_call/3 and handle_cast/2.
%%
%% Logs user out by destroying its player_fsm process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(internal, Uid, State) -> UpdatedState when
    Uid :: atom(),
    State :: map(),
    UpdatedState :: map().
logout(internal, Uid, #{?LOGGED_IN_UIDS_SET := LoggedInUidsSet} = State) ->
    UpdatedLoggedInUidsSet =
        case gb_sets:is_element(Uid, LoggedInUidsSet) of
            false ->
                LoggedInUidsSet;
            _ ->
                spawn(player_fsm, logout, [Uid]),
                gb_sets:del_element(Uid, LoggedInUidsSet)
        end,
    State#{?LOGGED_IN_UIDS_SET := UpdatedLoggedInUidsSet}.