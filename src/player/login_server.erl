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
-export([
    start_link/0,
    is_uid_registered/1,
    is_in_registration/1,
    register_uid/2,
    registration_done/1,
    delete_player/1,
    login/2,
    is_uid_logged_in/1,
    logout/2,
    is_id_registered/1,
    registered_player_uids/0,
    logout_all_players/0,
    logged_in_player_uids/0,
    not_logged_in_player_uids/0,
    uids_by_ids/1,
    login_state/0
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

-type uid_set() :: gb_sets:set(player_statem:uid()).

-include("../data_type/player_profile.hrl").

-record(state, {
    logged_in_uids_set :: uid_set(),
    registering_uids_set :: uid_set(),
    born_type_info_map :: register_statem:born_type_info_map(),
    uids_cross_ids :: [{player_statem:uid(), player_statem:id()}]
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
%% Checks if uid has been registered.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_uid_registered(Uid) -> boolean() when
    Uid :: player_statem:uid().
is_uid_registered(Uid) ->
    UidsCrossIds = gen_server:call(?SERVER, uids_cross_ids),
    lists:keymember(Uid, 1, UidsCrossIds).

%%--------------------------------------------------------------------
%% @doc
%% Checks if id has been registered.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_id_registered(Id) -> boolean() when
    Id :: player_statem:id().
is_id_registered(Id) ->
    UidsCrossIds = gen_server:call(?SERVER, uids_cross_ids),
    lists:keymember(Id, 2, UidsCrossIds).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves all registered player uids.
%%
%% @end
%%--------------------------------------------------------------------
-spec registered_player_uids() -> [player_statem:uid()].
registered_player_uids() ->
    UidsCrossIds = gen_server:call(?SERVER, uids_cross_ids),
    [Uid || {Uid, _Id} <- UidsCrossIds].

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid is in registration procedure.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_in_registration(Uid) -> boolean() when
    Uid :: player_statem:uid().
is_in_registration(Uid) ->
    RegisteringUidsSet = gen_server:call(?SERVER, registering_uids_set),
    gb_sets:is_element(Uid, RegisteringUidsSet).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid is in registration procedure.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_uid(DispatcherPid, Uid) -> ok when
    Uid :: player_statem:uid(),
    DispatcherPid :: pid().
register_uid(DispatcherPid, Uid) ->
    spawn(
        fun() ->
            #state{
                born_type_info_map = BornTypeInfoMap
            } = State = login_state(),
            UpdatedState =
                case register_statem_sup:add_child(DispatcherPid, Uid, BornTypeInfoMap) of
                    {ok, _Pid} ->
                        error_logger:info_msg("Started register fsm successfully.~nUid:~p~n", [Uid]),
                        State#state{
                            registering_uids_set = gb_sets:add(Uid, State#state.registering_uids_set)
                        };
                    {error, Reason} ->
                        error_logger:error_msg("Failed to start register fsm.~nUid:~p~nReason:~p~n", [Uid, Reason]),
                        State
                end,
            gen_server:cast(?SERVER, {update_state, UpdatedState})
        end
    ),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function is only called by regsiter_fsm when registration
%% procedure is done. It adds uid to registered uid set, save to
%% redis immediately, and log user in.
%%
%% @end
%%--------------------------------------------------------------------
-spec registration_done(PlayerProfile) -> ok when
    PlayerProfile :: #player_profile{}.
registration_done(#player_profile{
    uid = Uid,
    id = Id
} = PlayerProfile) ->
    #state{
        registering_uids_set = RegisteringUidsSet,
        uids_cross_ids = UidsCrossIds
    } = State = login_state(),
    UpdatedUidsCrossIds = [{Uid, Id} | UidsCrossIds],

    ok = redis_client_server:async_set(uids_cross_ids, UpdatedUidsCrossIds, false),
    true = redis_client_server:set(Uid, PlayerProfile, true),

    UpdatedState = State#state{
        uids_cross_ids = UpdatedUidsCrossIds,
        registering_uids_set = gb_sets:del_element(Uid, RegisteringUidsSet)
    },
    gen_server:cast(?SERVER, {update_state, UpdatedState}).

%%--------------------------------------------------------------------
%% @doc
%% This function deletes player in all states including redis. It logs
%% out player, delete player states from server state, and updates redis.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_player(Uid) -> ok when
    Uid :: player_statem:uid().
delete_player(Uid) ->
    #state{
        uids_cross_ids = UidsCrossIds
    } = State = login_state(),
    LoggedOutState = logout(internal, Uid, State),
    ok = elib:until_process_terminated(Uid),
    UpdatedUidsCrossIds = lists:keydelete(Uid, 1, UidsCrossIds),

    ok = redis_client_server:async_del([Uid], false),
    ok = redis_client_server:async_set(uids_cross_ids, UpdatedUidsCrossIds, true),

    gen_server:cast(?SERVER, {update_state, LoggedOutState#state{
        uids_cross_ids = UpdatedUidsCrossIds
    }}).

%%--------------------------------------------------------------------
%% @doc
%% Logs user in by creating its own player_statem process.
%%
%% @end
%%--------------------------------------------------------------------
-spec login(DispatcherPid, Uid) -> ok when
    DispatcherPid :: pid(),
    Uid :: player_statem:uid().
login(DispatcherPid, Uid) ->
    #state{
        logged_in_uids_set = LoggedInUidsSet
    } = State = login_state(),
    UpdatedLoggedInUidsSet =
        case gb_sets:is_element(Uid, LoggedInUidsSet) of
            false ->
                case player_statem_sup:add_child(Uid, DispatcherPid) of
                    {ok, _Pid} ->
                        ok;
                    Exception ->
                        throw(Exception)
                end,
                gb_sets:add(Uid, LoggedInUidsSet);
            true ->
                ok = player_statem:response_content(Uid, [{nls, already_login}], DispatcherPid),
                LoggedInUidsSet
        end,

    gen_server:cast(?SERVER, {update_state, State#state{
        logged_in_uids_set = UpdatedLoggedInUidsSet
    }}).

%%--------------------------------------------------------------------
%% @doc
%% Checks if uid has logged in.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_uid_logged_in(Uid) -> boolean() when
    Uid :: player_statem:uid().
is_uid_logged_in(Uid) ->
    LoggedInUidsSet = gen_server:call(?SERVER, logged_in_uids_set),
    gb_sets:is_element(Uid, LoggedInUidsSet).

%%--------------------------------------------------------------------
%% @doc
%% Logs user out by destroying its player_statem process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(DispatcherPid, Uid) -> ok when
    DispatcherPid :: pid(),
    Uid :: player_statem:uid().
logout(DispatcherPid, Uid) ->
    State = login_state(),
    ok = player_statem:response_content(Uid, [{nls, already_logout}], DispatcherPid),
    UpdatedState = logout(internal, Uid, State),
    gen_server:cast(?SERVER, {update_state, UpdatedState}).

%%--------------------------------------------------------------------
%% @doc
%% Force logout all players. This is function is called when stopping server only.
%% @see cm:q/0.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout_all_players() -> ok.
logout_all_players() ->
    LoggedInUidsSet = gen_server:call(?SERVER, logged_in_uids_set),
    logout_all_players(LoggedInUidsSet).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves all logged in player uids.
%%
%% @end
%%--------------------------------------------------------------------
-spec logged_in_player_uids() -> uid_set().
logged_in_player_uids() ->
    gen_server:call(?SERVER, logged_in_uids_set).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves all none logged in player uids.
%%
%% @end
%%--------------------------------------------------------------------
-spec not_logged_in_player_uids() -> [player_statem:uid()].
not_logged_in_player_uids() ->
    #state{
        logged_in_uids_set = LoggedInUidsSet,
        uids_cross_ids = UidsCrossIds
    } = login_state(),
    [Uid || {Uid, _Id} <- UidsCrossIds, not gb_sets:is_element(Uid, LoggedInUidsSet)].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve uids by ids.
%%
%% @end
%%--------------------------------------------------------------------
-spec uids_by_ids([player_statem:id()]) -> [player_statem:uid()].
uids_by_ids(PlayerIds) ->
    UidsCrossIds = gen_server:call(?SERVER, uids_cross_ids),
    [Uid || {Uid, Id} <- UidsCrossIds, lists:member(Id, PlayerIds)].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve State.
%%
%% @end
%%--------------------------------------------------------------------
-spec login_state() -> #state{}.
login_state() ->
    gen_server:call(?SERVER, login_state).

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
-spec init([]) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    io:format("~p starting...", [?MODULE]),

    UidsCrossIds =
        case redis_client_server:get(uids_cross_ids) of
            undefined ->
                NewUidsCrossIds = [],
                true = redis_client_server:set(uids_cross_ids, NewUidsCrossIds, true),
                NewUidsCrossIds;
            ExistingUidsCrossIds ->
                ExistingUidsCrossIds
        end,

    BornTypeInfoMap = common_server:runtime_data(born_type_info),
    State = #state{
        registering_uids_set = gb_sets:new(),
        logged_in_uids_set = gb_sets:new(),
        born_type_info_map = BornTypeInfoMap,
        uids_cross_ids = UidsCrossIds
    },

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
    login_state |
    uids_cross_ids |
    registering_uids_set |
    logged_in_uids_set,

    Reply ::
    uid_set() |
    State |
    ok,

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(login_state, _From, State) ->
    {reply, State, State};
handle_call(uids_cross_ids, _From, #state{
    uids_cross_ids = UidsCrossIds
} = State) ->
    {reply, UidsCrossIds, State};
handle_call(registering_uids_set, _From, #state{
    registering_uids_set = RegisteringUidsSet
} = State) ->
    {reply, RegisteringUidsSet, State};
handle_call(logged_in_uids_set, _From, #state{
    logged_in_uids_set = LoggedInUidsSet
} = State) ->
    {reply, LoggedInUidsSet, State}.

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

    Request ::
    {update_state, NewState} |
    stop,

    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast({update_state, NewState}, _OldState) ->
    {noreply, NewState};
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
code_change(_OldVsn, State, _Extra) ->
    try
        UpdatedState = temp_player_data_update(State),
        {ok, UpdatedState}
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
%% This function is called by "delete_user" and "logout" from
%% handle_call/3 and handle_cast/2.
%%
%% Logs user out by destroying its player_statem process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(internal, Uid, State) -> UpdatedState when
    Uid :: player_statem:uid(),
    State :: #state{},
    UpdatedState :: State.
logout(
    internal,
    Uid,
    #state{
        logged_in_uids_set = LoggedInUidsSet
    } = State
) ->
    UpdatedLoggedInUidsSet =
        case gb_sets:is_element(Uid, LoggedInUidsSet) of
            false ->
                LoggedInUidsSet;
            true ->
                ok = player_statem:logout(Uid),
                gb_sets:del_element(Uid, LoggedInUidsSet)
        end,
    State#state{
        logged_in_uids_set = UpdatedLoggedInUidsSet
    }.

%%--------------------------------------------------------------------
%% @doc
%% Logout all players.
%% @see logout_all_players/0.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout_all_players(LoggedInUidsSet) -> ok when
    LoggedInUidsSet :: uid_set().
logout_all_players(LoggedInUidsSet) ->
    case gb_sets:is_empty(LoggedInUidsSet) of
        true ->
            ok;
        false ->
            Iter = gb_sets:iterator(LoggedInUidsSet),
            do_logout_all_players(gb_sets:next(Iter))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Logout all players.
%% @see logout_all_players/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_logout_all_players(Next) -> ok when
    Next :: {Uid, Iter} | none,
    Uid :: player_statem:uid(),
    Iter :: gb_sets:iter().
do_logout_all_players({Uid, Iter}) ->
    ok = player_statem:logout(Uid),
    do_logout_all_players(gb_sets:next(Iter));
do_logout_all_players(none) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Temporary code for handling data change for logged in players.
%%
%% @end
%%--------------------------------------------------------------------
-spec temp_player_data_update(State) -> UpdatedState when
    State :: #state{} | tuple(), % generic tuple
    UpdatedState :: State.
temp_player_data_update(State) ->
    State.