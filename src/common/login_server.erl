%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
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
    remove_user/1,
    bringup_registration/2,
    login/2,
    is_uid_logged_in/1,
    logout/2]).

-define(R_REGISTERED_UID_LIST, registered_uid_list).
-define(REGISTERING_UID_LIST, registration_uid_list).
-define(LOGGED_IN_UID_LIST, logged_in_uid_list).

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
%% Starts the server
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
%% Check if uid is registered
%%
%% @end
%%--------------------------------------------------------------------
-spec is_uid_registered(Uid) -> boolean() when
    Uid :: atom().
is_uid_registered(Uid) ->
    gen_server:call(?MODULE, {is_uid_registered, Uid}).

%%--------------------------------------------------------------------
%% @doc
%% Check if uid is in registration process
%%
%% @end
%%--------------------------------------------------------------------
-spec is_in_registration(Uid) -> boolean() when
    Uid :: atom().
is_in_registration(Uid) ->
    gen_server:call(?MODULE, {is_in_registration, Uid}).

-spec register_uid(DispatcherPid, Uid) -> no_return() when
    Uid :: atom(),
    DispatcherPid :: pid().
register_uid(DispatcherPid, Uid) ->
    gen_server:cast(?MODULE, {register_uid, DispatcherPid, Uid}).

-spec registration_done(State, DispatcherPid) -> ok when
    State :: map(),
    DispatcherPid :: pid().
registration_done(State, DispatcherPid) ->
    gen_server:cast(?MODULE, {registration_done, State, DispatcherPid}),
    ok.

-spec remove_user(Uid) -> no_return() when
    Uid :: atom().
remove_user(Uid) ->
    gen_server:call(?MODULE, {remove_user, Uid}).

-spec bringup_registration(StateName, StateData) -> ok when
    StateName :: atom(),
    StateData :: map().
bringup_registration(StateName, StateData) ->
    gen_server:cast(?MODULE, {bringup_registration, StateName, StateData}).

-spec login(DispatcherPid, Uid) -> ok when
    DispatcherPid :: pid(),
    Uid :: atom().
login(DispatcherPid, Uid) ->
    gen_server:cast(?MODULE, {login, DispatcherPid, Uid}).

-spec is_uid_logged_in(Uid) -> boolean() when
    Uid :: atom().
is_uid_logged_in(Uid) ->
    gen_server:call(?MODULE, {is_uid_logged_in, Uid}).

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
    State :: map(),
    Reason :: term().
init([]) ->
    io:format("~p starting~n", [?MODULE]),
    RegisteredUidList =
        case redis_client_server:get(?R_REGISTERED_UID_LIST) of
            undefined ->
                NewList = [],
                redis_client_server:set(?R_REGISTERED_UID_LIST, NewList, true),
                NewList;
            UidList ->
                error_logger:info_msg("Registered uid list:~p~n", [UidList]),
                UidList
        end,
    {ok, #{?REGISTERING_UID_LIST => [], ?R_REGISTERED_UID_LIST => RegisteredUidList, ?LOGGED_IN_UID_LIST => []}}.

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

    Request :: {is_uid_registered | is_in_registration | remove_user, Uid},
    Uid :: atom(),
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({is_uid_registered, Uid}, _From, State) ->
    RegisteredUidList = maps:get(?R_REGISTERED_UID_LIST, State),
    {reply, common_api:list_has_element(RegisteredUidList, Uid), State};
handle_call({is_in_registration, Uid}, _From, State) ->
    RegisteringUidList = maps:get(?REGISTERING_UID_LIST, State),
    Result = common_api:list_has_element(RegisteringUidList, Uid),
    {reply, Result, State};
handle_call({remove_user, Uid}, _From, State) ->
    RegisteredUidList = maps:get(?R_REGISTERED_UID_LIST, State),
    UpdatedRegisteredUidList = lists:delete(Uid, RegisteredUidList),

    redis_client_server:async_del([Uid], false),
    redis_client_server:async_set(?R_REGISTERED_UID_LIST, UpdatedRegisteredUidList, true),

    {reply, ok, State#{?R_REGISTERED_UID_LIST := UpdatedRegisteredUidList}};
handle_call({is_uid_logged_in, Uid}, _From, State) ->
    LoggedUidList = maps:get(?LOGGED_IN_UID_LIST, State),
    {reply, common_api:list_has_element(LoggedUidList, Uid), State}.

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

    Request :: {registration_done, PlayerProfile, DispatcherPid} | {register_uid, DispatcherPid, Uid} | {bringup_registration, StateName, StateData} | {login, DispatcherPid, Uid},
    DispatcherPid :: pid(),
    Uid :: atom(),
    PlayerProfile :: command_dispatcher:uid_profile(),
    State :: map(),
    NewState :: map(),
    Reason :: term(),
    StateName :: atom(),
    StateData :: map().
handle_cast({registration_done, PlayerProfile, DispatcherPid}, State) ->
    Uid = maps:get(uid, PlayerProfile),
    UpdatedRegisteredUidList = [Uid] ++ maps:get(?R_REGISTERED_UID_LIST, State),

    redis_client_server:async_set(Uid, PlayerProfile, false),
    redis_client_server:async_set(?R_REGISTERED_UID_LIST, UpdatedRegisteredUidList, true),

    RegisteringUidList = maps:get(?REGISTERING_UID_LIST, State),
    UpdatedState = State#{?R_REGISTERED_UID_LIST := UpdatedRegisteredUidList, ?REGISTERING_UID_LIST => lists:delete(Uid, RegisteringUidList)},

    login(DispatcherPid, Uid),
    {noreply, UpdatedState};
handle_cast({register_uid, DispatcherPid, Uid}, State) ->
    register_fsm:start({init, DispatcherPid, Uid}),
    RegisteringUidList = maps:get(?REGISTERING_UID_LIST, State),
    {noreply, State#{?REGISTERING_UID_LIST := [Uid] ++ RegisteringUidList}};
handle_cast({bringup_registration, StateName, StateData}, State) ->
    spawn(fun() ->
        register_fsm:start({bringup, StateName, StateData})
    end),
    {noreply, State};
handle_cast({login, DispatcherPid, Uid}, State) ->
    LoggedInUidList = maps:get(?LOGGED_IN_UID_LIST, State),

    UpdatedLoggedInUidList =
        case common_api:index_of(LoggedInUidList, Uid) of
            -1 ->
                PlayerProfile = redis_client_server:get(Uid),
                #{scene := CurSceneName} = PlayerProfile,
                player_fsm:start({init, PlayerProfile}),
                scene_fsm:enter(CurSceneName, PlayerProfile, DispatcherPid),
                [Uid] ++ LoggedInUidList;
            _ ->
                player_fsm:send_message(Uid, login, [{nls, already_login}], DispatcherPid),
                LoggedInUidList
        end,

    {noreply, State#{?LOGGED_IN_UID_LIST := UpdatedLoggedInUidList}};
handle_cast({logout, DispatcherPid, Uid}, State) ->
    LoggedInUidList = maps:get(?LOGGED_IN_UID_LIST, State),
    Lang = player_fsm:get_lang(Uid),
    UpdatedLoggedInUidList =
        case common_api:index_of(LoggedInUidList, Uid) of
            -1 ->
                LoggedInUidList;
            _ ->
                player_fsm:leave_scene(Uid),
                player_fsm:stop(Uid),
                lists:delete(Uid, LoggedInUidList)
        end,

    nls_server:response_text(logout, [{nls, already_logout}], Lang, DispatcherPid),
    {noreply, State#{?LOGGED_IN_UID_LIST := UpdatedLoggedInUidList}}.

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