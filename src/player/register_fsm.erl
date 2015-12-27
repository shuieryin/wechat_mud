%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Registration gen_fsm. This gen_fsm states linear as
%%% select_lang --> input_id --> input_gender --> input_born_month --> input_confirmation --> done.
%%% This gen_fsm is created when player starts registration procedure
%%% and destroyed when the registration is done.
%%%
%%% @end
%%% Created : 29. Aug 2015 5:49 PM
%%%-------------------------------------------------------------------
-module(register_fsm).
-author("Shuieryin").

-behaviour(gen_fsm).

%% API
-export([
    start_link/2,
    input/3,
    select_lang/2,
    input_id/2,
    input_gender/2,
    input_born_month/2,
    input_confirmation/2,
    fsm_server_name/1,
    current_player_profile/1,
    start/2,
    stop/1
]).

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

-import(command_dispatcher, [return_content/2]).

-define(BORN_SCENE, dream_board_nw).
-define(ID_RULE_REGEX, "^[a-zA-Z0-9]{6,10}$").

-type state_name() :: select_lang | input_id | input_gender | input_born_month | input_confirmation | state_name.

-include("../data_type/player_profile.hrl").
-include("../data_type/npc_born_info.hrl").

-record(state, {self :: #player_profile{}, summary_content :: [nls_server:value()]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a player gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function creates a register gen_fsm with uid and the default
%% state, and prints all supported langauge abbreviations to user.
%% The server name is set to "uid+_register_fsm".
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(DispatcherPid, Uid) -> gen:start_ret() when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid().
start_link(DispatcherPid, Uid) ->
    gen_fsm:start_link({local, fsm_server_name(Uid)}, ?MODULE, {Uid, DispatcherPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Same as start_link/2 but without link.
%% @see start_link/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(DispatcherPid, Uid) -> gen:start_ret() when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid().
start(DispatcherPid, Uid) ->
    gen_fsm:start({local, fsm_server_name(Uid)}, ?MODULE, {Uid, DispatcherPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the register fsm
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Uid) -> ok when
    Uid :: player_fsm:uid().
stop(Uid) ->
    gen_fsm:send_all_state_event(fsm_server_name(Uid), stop).

%%--------------------------------------------------------------------
%% @doc
%% Executes states with player inputs. This function is called in
%% spawn_link(M, F, A) by command_dispatcher:pending_content/3.
%% @see command_dispatcher:pending_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec input(DispatcherPid, Uid, Input) -> ok when
    Uid :: player_fsm:uid(),
    Input :: binary(),
    DispatcherPid :: pid().
input(DispatcherPid, Uid, Input) ->
    gen_fsm:send_event(fsm_server_name(Uid), {Input, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Get player profile of the current state.
%%
%% @end
%%--------------------------------------------------------------------
-spec current_player_profile(Uid) -> CurrentPlayerProfile when
    Uid :: player_fsm:uid(),
    CurrentPlayerProfile :: #player_profile{}.
current_player_profile(Uid) ->
    gen_fsm:sync_send_all_state_event(fsm_server_name(Uid), current_player_profile).

%%--------------------------------------------------------------------
%% @doc
%% Appends "_register_fsm" to Uid as the register_fsm server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec fsm_server_name(Uid) -> RegisterFsmName when
    Uid :: player_fsm:uid(),
    RegisterFsmName :: erlang:registered_name().
fsm_server_name(Uid) ->
    list_to_atom(atom_to_list(Uid) ++ "_register_fsm").

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
%% See start_link/2 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec init({Uid, DispatcherPid}) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    Reason :: term(), % generic term
    StateName :: state_name(),
    StateData :: #state{}.
init({Uid, DispatcherPid}) ->
%%    error_logger:info_msg("register fsm init~nUid:~p~n", [Uid]),
    State = #state{self = #player_profile{uid = Uid}},
    nls_server:response_content([{nls, select_lang}], zh, DispatcherPid),
    {ok, select_lang, State}.

%%--------------------------------------------------------------------
%% @doc
%% Select language. This is the default init state in registration
%% procedure. If player input supported langauge abbreviatio, sets the
%% next state to "input_gender" and prints its state infos to player,
%% otherwise prints current state infos to player.
%%
%% After player has selected one of the supported langauges, all of
%% the subsequence messages will be displayed in the selected langauges
%% until player switched lanauge.
%% @see player_fsm:switch_lang/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_lang(Request, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Request :: {LangBin, DispatcherPid},
    LangBin :: binary(),

    DispatcherPid :: pid(),
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
select_lang({LangBin, DispatcherPid}, #state{self = PlayerProfile} = State) ->
    {NextState, NewState, ContentList, Lang} =
        try
            case nls_server:is_valid_lang(LangBin) of
                true ->
                    CurLang = binary_to_atom(LangBin, utf8),
                    {input_id, State#state{self = PlayerProfile#player_profile{lang = CurLang}}, [{nls, please_input_id}], CurLang};
                false ->
                    {select_lang, State, [{nls, select_lang}], zh}
            end
        catch
            _:_ ->
                {select_lang, State, [{nls, select_lang}], zh}
        end,
    nls_server:response_content(ContentList, Lang, DispatcherPid),
    {next_state, NextState, NewState}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player id. The previous state is "select_lang". If player
%% inputs valid gender, sets the next state to "input_gender" and
%% prints its state infos to player, otherwise prints current state
%% infos to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_id({RawId, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    RawId :: binary(),
    DispatcherPid :: pid(),
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
input_id({RawId, DispatcherPid}, #state{self = #player_profile{lang = Lang} = PlayerProfile} = State) ->
    % TODO: filter npc, items and prohibited names
    {MessageList, NextStateName, UpdatedState} =
        case re:run(RawId, ?ID_RULE_REGEX) of
            nomatch ->
                {[{nls, invalid_id}, RawId, <<"\n\n">>, {nls, please_input_id}], input_id, State};
            _ ->
                Id = list_to_binary(string:to_lower(binary_to_list(RawId))),
                case login_server:is_id_registered(Id) of
                    true ->
                        {[{nls, id_already_exists}, <<"\n\n">>, {nls, please_input_id}], input_id, State};
                    false ->
                        {[{nls, please_input_gender}], input_gender, State#state{self = PlayerProfile#player_profile{id = Id}}}
                end
        end,
    nls_server:response_content(MessageList, Lang, DispatcherPid),
    {next_state, NextStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player gender. The previous state is "select_lang". If player
%% inputs valid gender, sets the next state to "input_born_month" and
%% prints its state infos to player, otherwise prints current state
%% infos to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_gender({RawGender, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    RawGender :: binary(),
    DispatcherPid :: pid(),
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
input_gender({RawGender, DispatcherPid}, State) when RawGender == <<"m">> orelse RawGender == <<"male">> ->
    input_gender(male, DispatcherPid, State);
input_gender({RawGender, DispatcherPid}, State) when RawGender == <<"f">> orelse RawGender == <<"female">> ->
    input_gender(female, DispatcherPid, State);
input_gender({Other, DispatcherPid}, #state{self = #player_profile{lang = Lang}} = State) ->
    ErrorMessageNlsList =
        case Other of
            <<>> ->
                [{nls, please_input_gender}];
            SomeInput ->
                [{nls, invalid_gender}, SomeInput, <<"\n\n">>, {nls, please_input_gender}]
        end,
    nls_server:response_content(ErrorMessageNlsList, Lang, DispatcherPid),
    {next_state, input_gender, State}.
input_gender(Gender, DispatcherPid, #state{self = #player_profile{lang = Lang} = PlayerProfile} = State) ->
    nls_server:response_content([{nls, please_input_born_month}], Lang, DispatcherPid),
    {next_state, input_born_month, State#state{self = PlayerProfile#player_profile{gender = Gender}}}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player born month. The previous state is "input_gender". If
%% player inputs valid born month, sets the next state to
%% "input_confirmation" and prints its state infos with all summary
%% infos to player, otherwise prints current state infos to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_born_month({MonthBin, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when
    MonthBin :: binary(),
    DispatcherPid :: pid(),
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
input_born_month({MonthBin, DispatcherPid}, #state{self = #player_profile{lang = Lang} = PlayerProfile} = State) ->
    {NewStateName, NewState, ContentList} =
        case validate_month(MonthBin) of
            {ok, Month} ->
                UpdatedPlayerProfile = PlayerProfile#player_profile{born_month = Month},
                SummaryContent = gen_summary_content(UpdatedPlayerProfile),
                {input_confirmation, State#state{self = UpdatedPlayerProfile, summary_content = SummaryContent}, SummaryContent};
            {false, MonthBin} ->
                ErrorMessageNlsContent =
                    case MonthBin of
                        <<>> ->
                            [];
                        SomeInput ->
                            [{nls, invalid_month}, SomeInput, <<"\n\n">>]
                    end,
                {input_born_month, State, [ErrorMessageNlsContent, {nls, please_input_born_month}]}
        end,
    nls_server:response_content(lists:flatten(ContentList), Lang, DispatcherPid),
    {next_state, NewStateName, NewState}.

%%--------------------------------------------------------------------
%% @doc
%% Acknowledgement of all the preivous inputs. The previous state is
%% "input_born_month". If player inputs "y" or "n", tells login server
%% the registration is done by calling login_server:registration_done/2
%% and terminates the register gen_fsm.
%% @see login_server:registration_done/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_confirmation({Event, DispatcherPid}, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    DispatcherPid :: pid(),

    State :: #state{},
    Event :: binary(),
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
input_confirmation({Answer, DispatcherPid}, #state{self = PlayerProfile} = State) when Answer == <<"y">> orelse Answer == <<"yes">> ->
    #npc_born_info{name_nls_key = StubNpcNameNlsKey, description_nls_key = DescriptionNlsKey, self_description_nls_key = SelfDescriptionNlsKey} = common_server:random_npc(),

    FinalPlayerProfile = PlayerProfile#player_profile{register_time = cm:timestamp(), scene = ?BORN_SCENE, name = {nls, StubNpcNameNlsKey}, description = {nls, DescriptionNlsKey}, self_description = {nls, SelfDescriptionNlsKey}},
    UpdatedState = State#state{self = FinalPlayerProfile},

    login_server:registration_done(FinalPlayerProfile, DispatcherPid),
    {stop, normal, UpdatedState};
input_confirmation({Answer, DispatcherPid}, #state{self = #player_profile{lang = Lang, uid = Uid}}) when Answer == <<"n">> orelse Answer == <<"no">> ->
    nls_server:response_content([{nls, please_input_id}], Lang, DispatcherPid),
    {next_state, input_id, #state{self = #player_profile{uid = Uid, lang = Lang}}};
input_confirmation({Other, DispatcherPid}, #state{self = #player_profile{lang = Lang}, summary_content = SummaryContent} = State) ->
    ErrorMessageNlsContent
        = case Other of
              <<>> ->
                  [SummaryContent];
              _ ->
                  lists:flatten([{nls, invalid_command}, Other, <<"\n\n">>, SummaryContent])
          end,
    nls_server:response_content(ErrorMessageNlsContent, Lang, DispatcherPid),
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

    Event :: term(), % generic term
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
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

    Event :: term(), % generic term
    Reply :: ok,

    From :: {pid(), term()}, % generic term
    State :: #state{},
    NextStateName :: state_name(),
    NextState :: State,
    Reason :: normal | term(), % generic term
    NewState :: State.
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
-spec handle_event(Event, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when

    Event :: stop,
    Reason :: normal,

    StateName :: state_name(),
    StateData :: #state{},
    NextStateName :: StateName,
    NewStateData :: StateData.
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

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

    Event :: current_player_profile,
    Reply :: #player_profile{},

    From :: {pid(), Tag :: term()}, % generic term
    StateName :: state_name(),
    StateData :: #state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_sync_event(current_player_profile, _From, StateName, #state{self = CurrentPlayerProfile} = State) ->
    {reply, CurrentPlayerProfile, StateName, State}.

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

    Info :: term(), % generic term
    StateName :: state_name(),
    StateData :: #state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: normal | term(). % generic term
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored. If the gen_fsm is terminated
%% abnormally, it is restarted with the current state name and state data.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, StateName, StateData) -> ok when
    Reason :: normal | done | shutdown | {shutdown, term()} | term(), % generic term
    StateName :: state_name(),
    StateData :: #state{}.
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData} when
    OldVsn :: term() | {down, term()}, % generic term
    StateName :: state_name(),
    StateData :: #state{},
    Extra :: term(), % generic term
    NextStateName :: state_name(),
    NewStateData :: StateData.
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
    PDict :: [{Key :: term(), Value :: term()}], % generic term
    State :: #state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_fsm:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Validates the input month and return integer month if
%% validation passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_month(MonthBin) -> {ok, Month} | {false, MonthBin} when
    MonthBin :: binary(),
    Month :: player_fsm:born_month().
validate_month(MonthBin) ->
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

%%--------------------------------------------------------------------
%% @doc
%% Generates summary of all player inputs.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_summary_content(PlayerProfile) -> Content when
    PlayerProfile :: #player_profile{},
    Content :: [nls_server:value()].
gen_summary_content(PlayerProfile) ->
    [_ | RecordValues] = tuple_to_list(PlayerProfile),
    RecordFieldNames = record_info(fields, player_profile),
    lists:reverse([{nls, is_confirmed}, <<"\n">> | gen_summary_content(RecordFieldNames, RecordValues, [])]).

%%--------------------------------------------------------------------
%% @doc
%% Implementation for function gen_summary_content/1.
%% @see gen_summary_content/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_summary_content(FieldNames, Values, AccContent) -> Content when
    FieldNames :: [atom()], % generic atom
    Values :: [term()], % generic term
    AccContent :: [nls_server:value()],
    Content :: AccContent.
gen_summary_content([FieldName | RestFieldNames], [Value | RestValues], AccSummary)
    when FieldName == id;
    FieldName == gender;
    FieldName == born_month ->
    UpdatedAccSummary = [<<"\n">>, gen_summary_convert_value(FieldName, Value), {nls, list_to_atom(atom_to_list(FieldName) ++ "_label")} | AccSummary],
    gen_summary_content(RestFieldNames, RestValues, UpdatedAccSummary);
gen_summary_content([_ | RestFieldNames], [_ | RestValues], AccSummary) ->
    gen_summary_content(RestFieldNames, RestValues, AccSummary);
gen_summary_content([], [], AccSummary) ->
    AccSummary.

%%--------------------------------------------------------------------
%% @doc
%% Converts value for generate summary only. Converts raw value to
%% binary when it is integer, converts value to {nls, value} when
%% it is atom, and returns the raw value for the rest of types.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_summary_convert_value(Key, Value) -> ConvertedValue when
    Key :: atom(), % generic atom
    Value :: term(), % generic term
    ConvertedValue :: Value.
gen_summary_convert_value(id, Value) ->
    Value;
gen_summary_convert_value(_, Value) when is_integer(Value) ->
    integer_to_binary(Value);
gen_summary_convert_value(_, Value) when is_atom(Value) ->
    {nls, Value};
gen_summary_convert_value(_, Value) ->
    Value.