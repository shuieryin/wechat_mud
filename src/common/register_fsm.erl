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
-export([start/1,
    input/3,
    select_lang/2,
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

-import(command_dispatcher, [return_text/2]).

-define(STATE_NAMES, [{gender, gender_label}, {born_month, born_month_label}]).
-define(BORN_SCENE, dream_board_nw).

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
-spec start(Request) -> {ok, pid()} | ignore | {error, Reason} when
    Request :: {init, DispatcherPid, Uid} | {bringup, StateName, StateData},
    Uid :: atom(),
    DispatcherPid :: pid(),
    StateName :: atom(),
    StateData :: map(),
    Reason :: term().
start({init, DispatcherPid, Uid}) ->
    nls_server:response_text(?MODULE, [{nls, select_lang}], zh, DispatcherPid),
    gen_fsm:start({local, module_name(Uid)}, ?MODULE, [{init, Uid}], []);
start({bringup, StateName, StateData}) ->
    Uid = maps:get(uid, StateData),
    gen_fsm:start({local, module_name(Uid)}, ?MODULE, [{bringup, StateName, StateData}], []).

-spec input(DispatcherPid, Uid, Input) -> ok when
    Uid :: atom(),
    Input :: string(),
    DispatcherPid :: pid().
input(DispatcherPid, Uid, Input) ->
    gen_fsm:send_event(module_name(Uid), {Input, DispatcherPid}).

-spec stop(State) -> no_return() when
    State :: map().
stop(State) ->
    Uid = maps:get(uid, State),
    gen_fsm:send_all_state_event(module_name(Uid), stop).


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
-spec init([Request]) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Request :: {init, Uid} | {bringup, StateName, StateData},
    Uid :: atom(),
    StateName :: atom(),
    StateData :: map(),
    Reason :: term().
init([{init, Uid}]) ->
    error_logger:info_msg("register fsm init~n", []),
    {ok, select_lang, #{uid => Uid}};
init([{bringup, StateName, StateData}]) ->
    error_logger:info_msg("register fsm bringup~n", []),
    {ok, StateName, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select language
%%
%% @end
%%--------------------------------------------------------------------
-spec select_lang({Lang, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when
    Lang :: binary(),
    DispatcherPid :: pid(),
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
select_lang({LangBin, DispatcherPid}, State) ->
    {NextState, NewState, TextList, Lang} =
        try
            CurLang = binary_to_atom(LangBin, utf8),
            case nls_server:is_valid_lang(?MODULE, CurLang) of
                true ->
                    {input_gender, State#{lang => CurLang}, [{nls, please_input_gender}], CurLang};
                _ ->
                    {select_lang, State, [{nls, select_lang}], zh}
            end
        catch
            _:_ ->
                {select_lang, State, [{nls, select_lang}], zh}
        end,
    nls_server:response_text(?MODULE, TextList, Lang, DispatcherPid),
    {next_state, NextState, NewState}.

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
input_gender({Gender, DispatcherPid}, State) when Gender == <<"m">> orelse Gender == <<"M">> ->
    input_gender(male, DispatcherPid, State);
input_gender({Gender, DispatcherPid}, State) when Gender == <<"f">> orelse Gender == <<"F">> ->
    input_gender(female, DispatcherPid, State);
input_gender({Other, DispatcherPid}, State) ->
    Lang = maps:get(lang, State),
    InvalidText = case Other of
                      <<>> ->
                          [];
                      SomeInput ->
                          [{nls, invalid_gender}, SomeInput, <<"\n\n">>]
                  end,
    nls_server:response_text(?MODULE, lists:flatten([InvalidText, {nls, please_input_gender}]), Lang, DispatcherPid),
    {next_state, input_gender, State}.
input_gender(Gender, DispatcherPid, State) ->
    Lang = maps:get(lang, State),
    nls_server:response_text(?MODULE, [{nls, please_input_born_month}], Lang, DispatcherPid),
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
    NextStateName :: input_confirmation | input_born_month,
    NextState :: map(),
    NewState :: map(),
    Reason :: term().
input_born_month({MonthBin, DispatcherPid}, State) ->
    Lang = maps:get(lang, State),
    {NewStateName, NewState, TextList} =
        case valid_month(MonthBin) of
            {ok, Month} ->
                UpdatedState = maps:put(born_month, Month, State),
                GenText = gen_summary_text(?STATE_NAMES, [], UpdatedState),
                {input_confirmation, UpdatedState#{confirmation_text => GenText}, GenText};
            {false, MonthBin} ->
                InvalidText = case MonthBin of
                                  <<>> ->
                                      [];
                                  SomeInput ->
                                      [{nls, invalid_month}, SomeInput, <<"\n\n">>]
                              end,
                {input_born_month, State, [InvalidText, {nls, please_input_born_month}]}
        end,
    nls_server:response_text(?MODULE, lists:flatten(TextList), Lang, DispatcherPid),
    {next_state, NewStateName, NewState}.

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

-spec gen_summary_text(StateNames, AccText, State) -> [binary()] when
    StateNames :: [{Key, Desc}],
    Key :: atom(),
    Desc :: atom(),
    AccText :: [any()],
    State :: map().
gen_summary_text([], AccText, _) ->
    [AccText, <<"\n">>, {nls, is_confirmed}];
gen_summary_text([{Key, Desc} | Tail], AccText, State) ->
    Value = value_to_binary(maps:get(Key, State)),
    gen_summary_text(Tail, [AccText, {nls, Desc}, Value, <<"\n">>], State).

value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value_to_binary(Value) when is_atom(Value) ->
    {nls, Value};
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
input_confirmation({Answer, DispatcherPid}, State) when Answer == <<"y">> orelse Answer == <<"Y">> ->
    State1 = State#{register_time => common_api:timestamp(), scene => ?BORN_SCENE},
    State2 = maps:remove(confirmation_text, State1),

    login_server:registration_done(State2, DispatcherPid),

    {stop, done, State2};
input_confirmation({Answer, DispatcherPid}, State) when Answer == <<"n">> orelse Answer == <<"N">> ->
    Lang = maps:get(lang, State),
    nls_server:response_text(?MODULE, [{nls, please_input_gender}], Lang, DispatcherPid),
    {next_state, input_gender, #{uid => maps:get(uid, State), lang => Lang}};
input_confirmation({Other, DispatcherPid}, State) ->
    Lang = maps:get(lang, State),
    InvalidText = case Other of
                      <<>> ->
                          [];
                      SomeInput ->
                          [{nls, invalid_command}, SomeInput, <<"\n\n">>]
                  end,
    nls_server:response_text(?MODULE, lists:flatten([InvalidText, maps:get(confirmation_text, State)]), Lang, DispatcherPid),
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
    Reason :: normal | done | shutdown | {shutdown, term()} | term(),
    StateName :: atom(),
    StateData :: term().
terminate(Reason, StateName, StateData) ->
    case Reason of
        Result when Result /= normal andalso Result /= done ->
            login_server:bringup_registration(StateName, StateData);
        _ ->
            ok
    end,
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
module_name(Uid) ->
    list_to_atom(atom_to_list(Uid) ++ "_fsm").