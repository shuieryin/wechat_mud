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
    turn_off_wechat_debug/0]).

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
    CommonConfig = case redis_client_server:get(?R_COMMON_CONFIG) of
                       undefined ->
                           NewConfig = #{},
                           redis_client_server:set(?R_COMMON_CONFIG, NewConfig, true),
                           NewConfig;
                       ReturnValue ->
                           ReturnValue
                   end,
    {ok, CommonConfig}.

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

    Request :: {is_wechat_debug} | {set_wechat_debug, boolean()},
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({is_wechat_debug}, _From, State) ->
    IsWechatDebug = maps:get(is_wechat_debug, State, ?DEFAULT_WECHAT_DEBUG_MODE),
    {reply, IsWechatDebug, State};
handle_call({set_wechat_debug, IsOn}, _From, State) ->
    {reply, IsOn, maps:put(is_wechat_debug, IsOn, State)}.

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
