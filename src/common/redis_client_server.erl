%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Redis client server. This server holds a connection with the redis
%%% server process and encapsulates APIs for interacting with the redis
%%% server.
%%%
%%% @end
%%% Created : 26. Aug 2015 10:00 AM
%%%-------------------------------------------------------------------
-module(redis_client_server).
-author("Shuieryin").

-behaviour(gen_server).

%% API
-export([start_link/0,
    reconnect_redis/0,
    get/1,
    set/3,
    save/0,
    async_set/3,
    async_save/0,
    del/2,
    async_del/2,
    clear_all/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-define(SERVER, ?MODULE).
-define(HOST, "127.0.0.1").
-define(PORT, 6379).

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
%% Reconnects redis server
%%
%% @end
%%--------------------------------------------------------------------
reconnect_redis() ->
    gen_server:call(?MODULE, connect_redis).

%%--------------------------------------------------------------------
%% @doc
%% Gets value from redis server
%%
%% @end
%%--------------------------------------------------------------------
-spec get(Key) -> ReturnValue when
    Key :: atom(),
    ReturnValue :: term() | undefined.
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Sets value to redis server. This function is blocked until the
%% value is set.
%%
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Value, IsSave) -> Result when
    Key :: term(),
    Value :: term(),
    IsSave :: boolean(),
    Result :: boolean().
set(Key, Value, IsSave) ->
    gen_server:call(?MODULE, {set, Key, Value, IsSave}).

%%--------------------------------------------------------------------
%% @doc
%% Asynchronously sets value to redis server.
%%
%% @end
%%--------------------------------------------------------------------
-spec async_set(Key, Value, IsSave) -> ok when
    Key :: term(),
    Value :: any(),
    IsSave :: boolean().
async_set(Key, Value, IsSave) ->
    gen_server:cast(?MODULE, {set, Key, Value, IsSave}).

%%--------------------------------------------------------------------
%% @doc
%% Saves current redis dataset to RDB file. This function is blocked until the
%% value is set.
%%
%% @end
%%--------------------------------------------------------------------
-spec save() -> ok.
save() ->
    gen_server:call(?MODULE, save).

%%--------------------------------------------------------------------
%% @doc
%% Asynchronously saves current redis dataset to RDB file.
%%
%% @end
%%--------------------------------------------------------------------
-spec async_save() -> ok.
async_save() ->
    gen_server:cast(?MODULE, save).

%%--------------------------------------------------------------------
%% @doc
%% Deletes value in redis server. This function is blocked until the
%% value is set.
%%
%% @end
%%--------------------------------------------------------------------
-spec del(Keys, IsSave) -> Result when
    Keys :: [term()],
    IsSave :: boolean(),
    Result :: boolean().
del(Keys, IsSave) ->
    gen_server:call(?MODULE, {del, Keys, IsSave}).

%%--------------------------------------------------------------------
%% @doc
%% Asynchronously deletes value in redis server.
%%
%% @end
%%--------------------------------------------------------------------
-spec async_del(Keys, IsSave) -> ok when
    Keys :: [term()],
    IsSave :: boolean().
async_del(Keys, IsSave) ->
    gen_server:cast(?MODULE, {del, Keys, IsSave}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes all values in redis server and saves dataset to RDB file.
%%
%% @end
%%--------------------------------------------------------------------
clear_all() ->
    {ok, RedisClientPid} = eredis:start_link(),
    eredis:q(RedisClientPid, ["FLUSHDB"]),
    eredis:q(RedisClientPid, ["SAVE"]).

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
    {ok, connect_reids(#{})}.

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

    Request :: {get, Key} | {set, Key, Value, IsSave} | {del, Keys, IsSave} | connect_redis | save,
    Key :: term(),
    Keys :: [term()],
    Value :: term(),
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({get, Key}, _From, State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    Value = case eredis:q(RedisClientPid, ["GET", Key]) of
                {ok, undefined} ->
                    undefined;
                {ok, <<>>} ->
                    undefined;
                {ok, ReturnValue} ->
                    binary_to_term(ReturnValue);
                {error, Reason} ->
                    error_logger:error_msg("Redis get value failed:~p~n", [Reason]),
                    undefined
            end,
    {reply, Value, State};
handle_call({set, Key, Value, IsSave}, _From, State) ->
    IsSet = set(Key, Value, IsSave, State),
    {reply, IsSet, State};
handle_call(connect_redis, _From, State) ->
    {rpely, ok, connect_reids(State)};
handle_call(save, _From, State) ->
    save(State),
    {rpely, ok, State};
handle_call({del, Keys, IsSave}, _From, State) ->
    IsDel = del(Keys, IsSave, State),
    {reply, IsDel, State}.

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

    Request :: {set, Key, Value, IsSave} | {del, Keys, IsSave} | save,
    Key :: term(),
    Keys :: [term()],
    Value :: term(),
    IsSave :: boolean(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_cast({set, Key, Value, IsSave}, State) ->
    set(Key, Value, IsSave, State),
    {noreply, State};
handle_cast(save, State) ->
    save(State),
    {noreply, State};
handle_cast({del, Keys, IsSave}, State) ->
    del(Keys, IsSave, State),
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
%% Makes connection to redis server. This function is blocked until
%% the connection is established.
%%
%% @end
%%--------------------------------------------------------------------
-spec connect_reids(State) -> map() when
    State :: map().
connect_reids(State) ->
    RedisClientPid = connect_redis_loop(),
    maps:put(redis_client_pid, RedisClientPid, State).

%%--------------------------------------------------------------------
%% @doc
%% Makes connection to redis server loop in every second. This function
%% is blocked until the connection is established and is called by
%% connect_redis/1 only.
%%
%% @end
%%--------------------------------------------------------------------
-spec connect_redis_loop() -> ClientPid when
    ClientPid :: pid().
connect_redis_loop() ->
    SelfPid = self(),
    spawn(fun() ->
        make_connection(SelfPid)
    end),
    receive
        {connected, SelfPid, RedisClientPid} ->
            RedisClientPid
    after
        1000 ->
            spawn(fun() ->
                make_connection(SelfPid)
            end),
            connect_redis_loop()
    end.

%%--------------------------------------------------------------------
%% @doc
%% Makes connection to redis server. Print reason when error occurs.
%%
%% @end
%%--------------------------------------------------------------------
make_connection(ParentPid) ->
    case eredis:start_link(?HOST, ?PORT) of
        {ok, ClientPid} ->
            ParentPid ! {connected, ParentPid, ClientPid};
        {error, Reason} ->
            error_logger:error_msg("Connect redis server failed:~p~n", [Reason]),
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Saves dataset to RDB file. Print reason when error occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec save(State) -> ok | fail when
    State :: map().
save(State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    {ok, Result} = eredis:q(RedisClientPid, ["SAVE"]),
    case Result of
        <<"OK">> ->
            ok;
        Error ->
            error_logger:error_msg("Redis save failed:~p~n", [Error]),
            fail
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets value to redis server and saves dataset to RDB file if "IsSave"
%% is true. Print reason when error occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Value, IsSave, State) -> IsSet when
    Key :: term(),
    Value :: term(),
    IsSave :: boolean(),
    State :: map(),
    IsSet :: boolean().
set(Key, Value, IsSave, State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    IsSet = case eredis:q(RedisClientPid, ["SET", Key, term_to_binary(Value)]) of
                {ok, <<"OK">>} ->
                    true;
                {Type, Reason} ->
                    error_logger:error_msg("Redis set value failed~n Type:~p, Reason:~p~n", [Type, Reason]),
                    false
            end,
    case IsSave of
        true ->
            save(State);
        _ ->
            ok
    end,
    IsSet.

%%--------------------------------------------------------------------
%% @doc
%% Deletes value in redis server and saves dataset to RDB file if "IsSave"
%% is true. Print reason when error occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec del(Keys, IsSave, State) -> boolean() when
    Keys :: [term()],
    IsSave :: boolean(),
    State :: map().
del(Keys, IsSave, State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    IsDel = case eredis:q(RedisClientPid, ["DEL" | Keys]) of
                {ok, <<"OK">>} ->
                    true;
                {Type, Reason} ->
                    error_logger:error_msg("Redis del value failed~n Type:~p, Reason:~p~n", [Type, Reason]),
                    false
            end,
    case IsSave of
        true ->
            save(State);
        _ ->
            ok
    end,
    IsDel.