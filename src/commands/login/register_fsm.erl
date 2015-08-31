
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Aug 2015 5:49 PM
%%%-------------------------------------------------------------------
-module(register_fsm).
-author("Shuieryin").

-behaviour(gen_fsm).

%% API
-export([start/2,
    input/3,
    input_gender/2,
    input_born_month/2,
    input_confirmation/2,
    stop/1,
    valid_month/1]).

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

-define(SERVER, ?MODULE).
-define(STATE_NAMES, [{gender, <<"性别: "/utf8>>}, {born_month, <<"出生月份: "/utf8>>}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Uid, DispatcherPid) -> {ok, pid()} | ignore | {error, Reason} when
    Uid :: atom(),
    DispatcherPid :: pid(),
    Reason :: term().
start(DispatcherPid, Uid) ->
    gen_fsm:start({local, Uid}, ?MODULE, [DispatcherPid, Uid], []).

-spec input(DispatcherPid, Uid, Input) -> ok when
    Uid :: atom(),
    Input :: string(),
    DispatcherPid :: pid().
input(DispatcherPid, Uid, Input) ->
    error_logger:info_msg("Uid:~p~nInput:~p~n", [Uid, Input]),
    gen_fsm:send_event(Uid, {Input, DispatcherPid}).

-spec stop(State) -> no_return() when
    State :: map().
stop(State) ->
    Uid = maps:get(uid, State),
    gen_fsm:send_all_state_event(Uid, stop).


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
%% @end
%%--------------------------------------------------------------------
-spec init({DispatcherPid, Uid}) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when
    DispatcherPid :: pid(),
    Uid :: atom(),
    StateName :: atom(),
    StateData :: map(),
    Reason :: term().
init([DispatcherPid, Uid]) ->
    error_logger:info_msg("register fsm init~n", []),
    command_dispatcher:return_text(DispatcherPid, <<"请输入角色的性别"/utf8>>),
    {ok, input_gender, #{uid => Uid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Input gender
%%
%% @end
%%--------------------------------------------------------------------
-spec input_gender({Gender, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when
    Gender :: binary(),
    DispatcherPid :: pid(),
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
input_gender({<<"male">>, DispatcherPid}, State) ->
    input_gender(male, DispatcherPid, State);
input_gender({<<"female">>, DispatcherPid}, State) ->
    input_gender(female, DispatcherPid, State);
input_gender({Other, DispatcherPid}, State) ->
    command_dispatcher:return_text(DispatcherPid, [<<"无效性别: "/utf8>>, Other, <<"\n请输入角色的性别"/utf8>>]),
    {next_state, input_gender, State}.
input_gender(Gender, DispatcherPid, State) ->
    command_dispatcher:return_text(DispatcherPid, <<"请输入角色的出生月份"/utf8>>),
    {next_state, input_born_month, maps:put(gender, Gender, State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Input born month
%%
%% @end
%%--------------------------------------------------------------------
-spec input_born_month({MonthBin, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when
    MonthBin :: binary(),
    DispatcherPid :: pid(),
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
input_born_month({MonthBin, DispatcherPid}, State) ->
    case valid_month(MonthBin) of
        {ok, Month} ->
            NewState = maps:put(born_month, Month, State),
            GenText = gen_summary_text(?STATE_NAMES, <<>>, NewState),
            command_dispatcher:return_text(DispatcherPid, GenText),
            {next_state, input_confirmation, NewState#{gen_text => GenText}};
        {false, MonthBin} ->
            command_dispatcher:return_text(DispatcherPid, [<<"无效月份: "/utf8>>, MonthBin, <<"\n请输入角色的出生月份"/utf8>>]),
            {next_state, input_born_month, State}
    end.

-spec valid_month(MonthStr) -> {ok, 1..12} | false when
    MonthStr :: string().
valid_month(MonthBin) ->
    try
        case binary_to_integer(MonthBin) of
            Month when Month >= 1, Month =< 12 ->
                {ok, Month};
            _ ->
                {false, MonthBin}
        end
    catch
        _:_ ->
            {false, MonthBin}
    end.

-spec gen_summary_text(StateNames, AccText, State) -> binary() when
    StateNames :: [{Key, Desc}],
    Key :: atom(),
    Desc :: binary(),
    AccText :: list(),
    State :: map().
gen_summary_text([], AccText, _) ->
    [AccText, <<"\n确定吗?"/utf8>>];
gen_summary_text([{Key, Desc} | Tail], AccText, State) ->
    Value = value_to_binary(maps:get(Key, State)),
    gen_summary_text(Tail, [AccText, Desc, Value, <<"\n">>], State).

value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
value_to_binary(Value) ->
    Value.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Input confirmation
%%
%% @end
%%--------------------------------------------------------------------
-spec input_confirmation({Event, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when
    DispatcherPid :: pid(),
    State :: map(),
    Event :: binary(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
input_confirmation({<<"yes">>, DispatcherPid}, State) ->
    command_dispatcher:return_text(DispatcherPid, <<"欢迎加入"/utf8>>),
    {stop, done, State};
input_confirmation({<<"no">>, DispatcherPid}, State) ->
    command_dispatcher:return_text(DispatcherPid, <<"请输入角色的性别"/utf8>>),
    {next_state, input_gender, #{uid => maps:get(uid, State)}};
input_confirmation({Other, DispatcherPid}, State) ->
    command_dispatcher:return_text(DispatcherPid, [<<"无效指令: "/utf8>>, Other, <<"\n\n">>, maps:get(gen_text, State)]),
    {next_state, input_confirmation, State}.

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
-spec handle_event({restart, DispatcherPid}, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when
    DispatcherPid :: pid(),
    StateName :: atom(),
    StateData :: map(),
    NextStateName :: atom(),
    NewStateData :: map(),
    Reason :: term().
handle_event({restart, DispatcherPid}, _StateName, State) ->
    error_logger:info_msg("register fsm restarted", []),
    init([maps:get(uid, State), DispatcherPid]);
handle_event(_Event, StateName, State) ->
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
    Event :: term(),
    From :: {pid(), Tag :: term()},
    StateName :: atom(),
    StateData :: term(),
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
    Reason :: term().
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

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
terminate(_Reason, _StateName, StateData) ->
    login_server:registration_done(StateData),
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
