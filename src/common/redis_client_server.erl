%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
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
    save/0]).

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
%% Reconnect redis
%%
%% @end
%%--------------------------------------------------------------------
reconnect_redis() ->
    gen_server:call(?MODULE, {connect_redis}).

%%--------------------------------------------------------------------
%% @doc
%% get value
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
%% set value
%%
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Value, IsSave) -> Result when
    Key :: atom(),
    Value :: term(),
    IsSave :: boolean(),
    Result :: boolean().
set(Key, Value, IsSave) ->
    gen_server:call(?MODULE, {set, Key, Value, IsSave}).

%%--------------------------------------------------------------------
%% @doc
%% Save
%%
%% @end
%%--------------------------------------------------------------------
-spec save() -> no_return().
save() ->
    gen_server:call(?MODULE, {save}).

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

    Request :: {get, Key} | {set, Key, Value} | {connect_redis | save},
    Key :: term(),
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
                {ok, ReturnValue} ->
                    binary_to_term(ReturnValue);
                {error, Reason} ->
                    error_logger:error_msg("Redis get value failed:~p~n", [Reason]),
                    undefined
            end,
    {reply, Value, State};
handle_call({set, Key, Value, IsSave}, _From, State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    IsSet = case eredis:q(RedisClientPid, ["SET", Key, Value]) of
                {ok, <<"OK">>} ->
                    true;
                {Type, Reason} ->
                    error_logger:error_msg("Redis set value failed~n Type:~p, Reason:~p~n", [Type, Reason]),
                    false
            end,
    if
        IsSave == true ->
            save(State)
    end,
    {reply, IsSet, State};
handle_call({connect_redis}, _From, State) ->
    {rpely, ok, connect_reids(State)};
handle_call({save}, _From, State) ->
    save(State),
    {rpely, ok, State}.

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

    Request :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
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
-spec connect_reids(State) -> map() when
    State :: map().
connect_reids(State) ->
    SelfPid = self(),
    ConnectRedisPid = spawn(
        fun() ->
            connect_redis_loop(SelfPid)
        end
    ),
    ConnectRedisPid ! {connect, SelfPid},
    receive
        {connected, ConnectRedisPid, ClientPid} ->
            maps:put(redis_client_pid, ClientPid, State)
    end.

-spec connect_redis_loop(ParentPid) -> no_return() when
    ParentPid :: pid().
connect_redis_loop(ParentPid) ->
    receive
        {connect, ParentPid} ->
            case eredis:start_link() of
                {ok, ClientPid} ->
                    ParentPid ! {connected, self(), ClientPid};
                {error, Reason} ->
                    error_logger:error_msg("Connect redis server failed:~p~n", [Reason]),
                    connect_redis_loop(ParentPid)
            end
    after
        1000 ->
            self() ! {connect, ParentPid}
    end.

-spec save(State) -> no_return() when
    State :: map().
save(State) ->
    RedisClientPid = maps:get(redis_client_pid, State),
    {ok, Result} = eredis:q(RedisClientPid, ["SAVE"]),
    if
        Result /= <<"OK">> ->
            error_logger:error_msg("Redis save failed:~p~n", [Result])
    end.