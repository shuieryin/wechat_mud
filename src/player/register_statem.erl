%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Registration gen_statem. This gen_statem states linear as
%%% select_lang --> input_id --> input_gender --> input_born_month --> input_confirmation --> done.
%%% This gen_statem is created when player starts registration procedure
%%% and destroyed when the registration is done.
%%%
%%% @end
%%% Created : 29. Aug 2015 5:49 PM
%%%-------------------------------------------------------------------
-module(register_statem).
-author("Shuieryin").

-behaviour(gen_statem).

%% API
-export([
    start_link/3,
    input/3,
    select_lang/3,
    input_id/3,
    input_gender/3,
    input_born_month/3,
    input_confirmation/3,
    register_server_name/1,
    current_player_profile/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    format_status/2,
    callback_mode/0
]).

-import(command_dispatcher, [return_content/2]).

-define(BORN_SCENE, dream_board_nw).
-define(ID_RULE_REGEX, "^[a-zA-Z0-9]{6,10}$").

-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").

-type state_name() :: select_lang | input_id | input_gender | input_born_month | input_confirmation | state_name.
-type born_type_info_map() :: #{player_statem:born_month() => #born_type_info{}}.

-record(state, {
    self :: #player_profile{} | undefined,
    summary_content :: [nls_server:value()] | undefined,
    born_type_info_map :: born_type_info_map() | undefined
}).

-export_type([
    born_type_info_map/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a player gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function creates a register gen_statem with uid and the default
%% state, and prints all supported langauge abbreviations to user.
%% The server name is set to "uid+_register_statem".
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(DispatcherPid, Uid, BornTypeInfoMap) -> gen:start_ret() when
    Uid :: player_statem:uid(),
    DispatcherPid :: pid(),
    BornTypeInfoMap :: register_statem:born_type_info_map().
start_link(DispatcherPid, Uid, BornTypeInfoMap) ->
    gen_statem:start_link({local, register_server_name(Uid)}, ?MODULE, {Uid, DispatcherPid, BornTypeInfoMap}, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the register fsm
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Uid) -> ok when
    Uid :: player_statem:uid().
stop(Uid) ->
    gen_statem:call(register_server_name(Uid), stop).

%%--------------------------------------------------------------------
%% @doc
%% Executes states with player inputs. This function is called in
%% spawn_link(M, F, A) by command_dispatcher:pending_content/3.
%% @see command_dispatcher:pending_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec input(DispatcherPid, Uid, Input) -> ok when
    Uid :: player_statem:uid(),
    Input :: binary(),
    DispatcherPid :: pid().
input(DispatcherPid, Uid, Input) ->
    gen_statem:cast(register_server_name(Uid), {Input, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Get player profile of the current state.
%%
%% @end
%%--------------------------------------------------------------------
-spec current_player_profile(Uid) -> CurrentPlayerProfile when
    Uid :: player_statem:uid(),
    CurrentPlayerProfile :: #player_profile{}.
current_player_profile(Uid) ->
    gen_statem:call(register_server_name(Uid), current_player_profile).

%%--------------------------------------------------------------------
%% @doc
%% Appends "_register_statem" to Uid as the register_statem server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_server_name(Uid) -> PlayerServerName when
    Uid :: player_statem:uid(),
    PlayerServerName :: erlang:registered_name().
register_server_name(Uid) ->
    list_to_atom(atom_to_list(Uid) ++ "_register_statem").

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% See start_link/2 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec init({Uid, DispatcherPid, BornTypeInfoMap}) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Uid :: player_statem:uid(),
    DispatcherPid :: pid(),
    BornTypeInfoMap :: register_statem:born_type_info_map(),

    Reason :: term(), % generic term
    StateName :: state_name(),
    StateData :: #state{}.
init({Uid, DispatcherPid, BornTypeInfoMap}) ->
    State = #state{
        self = #player_profile{
            uid = Uid
        },
        born_type_info_map = BornTypeInfoMap
    },
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
%% @see player_statem:switch_lang/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_lang(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {LangBin, DispatcherPid} |
    current_player_profile,

    LangBin :: binary(),
    DispatcherPid :: pid(),

    State :: #state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
select_lang(
    cast,
    {LangBin, DispatcherPid},
    #state{
        self = PlayerProfile
    } = Data
) ->
    {NextState, UpdatedData, ContentList, Lang} =
        try
            case nls_server:is_valid_lang(LangBin) of
                true ->
                    CurLang = binary_to_atom(LangBin, utf8),
                    {input_id, Data#state{self = PlayerProfile#player_profile{lang = CurLang}}, [{nls, please_input_id}], CurLang};
                false ->
                    {select_lang, Data, [{nls, select_lang}], zh}
            end
        catch
            _ErrorType:_Reason:_Stacktrace ->
                {select_lang, Data, [{nls, select_lang}], zh}
        end,
    nls_server:response_content(ContentList, Lang, DispatcherPid),
    {next_state, NextState, UpdatedData};
select_lang(
    {call, From},
    current_player_profile,
    #state{
        self = CurrentPlayerProfile
    }
) ->
    {keep_state_and_data, {reply, From, CurrentPlayerProfile}}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player id. The previous state is "select_lang". If player
%% inputs valid gender, sets the next state to "input_gender" and
%% prints its state infos to player, otherwise prints current state
%% infos to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_id(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {RawId, DispatcherPid} |
    current_player_profile,

    RawId :: binary(),
    DispatcherPid :: pid(),

    State :: #state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
input_id(
    cast,
    {RawId, DispatcherPid},
    #state{
        self = #player_profile{
            lang = Lang
        } = PlayerProfile
    } = State
) ->
    % TODO: filter npc, items and prohibited names
    {MessageList, NextStateName, UpdatedData} =
        case re:run(RawId, ?ID_RULE_REGEX) of
            nomatch ->
                {[{nls, invalid_id}, RawId, <<"\n\n">>, {nls, please_input_id}], input_id, State};
            _Match ->
                Id = list_to_binary(string:to_lower(binary_to_list(RawId))),
                case login_server:is_id_registered(Id) of
                    true ->
                        {[{nls, id_already_exists}, <<"\n\n">>, {nls, please_input_id}], input_id, State};
                    false ->
                        {[{nls, please_input_gender}], input_gender, State#state{self = PlayerProfile#player_profile{id = Id}}}
                end
        end,
    nls_server:response_content(MessageList, Lang, DispatcherPid),
    {next_state, NextStateName, UpdatedData};
input_id(
    {call, From},
    current_player_profile,
    #state{
        self = CurrentPlayerProfile
    }
) ->
    {keep_state_and_data, {reply, From, CurrentPlayerProfile}}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player gender. The previous state is "select_lang". If player
%% inputs valid gender, sets the next state to "input_born_month" and
%% prints its state infos to player, otherwise prints current state
%% infos to player.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_gender(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {RawGender, DispatcherPid} |
    current_player_profile,

    RawGender :: binary(),
    DispatcherPid :: pid(),

    State :: #state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
input_gender(cast, {RawGender, DispatcherPid}, #state{
    self = #player_profile{
        lang = Lang
    } = PlayerProfile
} = Data) ->
    Gender = if
                 RawGender == <<"m">> orelse RawGender == <<"male">> ->
                     male;
                 RawGender == <<"f">> orelse RawGender == <<"female">> ->
                     female;
                 true ->
                     undefined
             end,

    case Gender of
        undefined ->
            ErrorMessageNlsList =
                case RawGender of
                    <<>> ->
                        [{nls, please_input_gender}];
                    SomeInput ->
                        [{nls, invalid_gender}, SomeInput, <<"\n\n">>, {nls, please_input_gender}]
                end,
            nls_server:response_content(ErrorMessageNlsList, Lang, DispatcherPid),
            {next_state, input_gender, Data};
        _ValidGender ->
            nls_server:response_content([{nls, please_input_born_month}], Lang, DispatcherPid),
            {
                next_state,
                input_born_month,
                Data#state{
                    self = PlayerProfile#player_profile{
                        gender = Gender
                    }
                }
            }
    end;
input_gender(
    {call, From},
    current_player_profile,
    #state{
        self = CurrentPlayerProfile
    }
) ->
    {keep_state_and_data, {reply, From, CurrentPlayerProfile}}.

%%--------------------------------------------------------------------
%% @doc
%% Inputs player born month. The previous state is "input_gender". If
%% player inputs valid born month, sets the next state to
%% "input_confirmation" and prints its state infos with all summary
%% infos to player, otherwise prints current state infos to player.
%%
%% @end
%%-------------------------------------------------------------------
-spec input_born_month(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {MonthBin, DispatcherPid} |
    current_player_profile,

    MonthBin :: binary(),
    DispatcherPid :: pid(),

    State :: #state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
input_born_month(
    cast,
    {MonthBin, DispatcherPid},
    #state{
        self = #player_profile{
            lang = Lang
        } = PlayerProfile
    } = State
) ->
    {NewStateName, UpdatedData, ContentList} =
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
    {next_state, NewStateName, UpdatedData};
input_born_month(
    {call, From},
    current_player_profile,
    #state{
        self = CurrentPlayerProfile
    }
) ->
    {keep_state_and_data, {reply, From, CurrentPlayerProfile}}.

%%--------------------------------------------------------------------
%% @doc
%% Acknowledgement of all the preivous inputs. The previous state is
%% "input_born_month". If player inputs "y" or "n", tells login server
%% the registration is done by calling login_server:registration_done/2
%% and terminates the register gen_statem.
%% @see login_server:registration_done/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_confirmation(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {Answer, DispatcherPid} |
    current_player_profile,

    Answer :: binary(),
    DispatcherPid :: pid(),

    State :: #state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
input_confirmation(
    cast,
    {Answer, DispatcherPid},
    #state{
        self = #player_profile{
            uid = PlayerUid,
            born_month = BornMonth,
            gender = Gender
        } = PlayerProfile,
        born_type_info_map = BornTypeInfoMap
    } = State
) when Answer == <<"y">> orelse Answer == <<"yes">> ->
    #born_type_info{
        strength = M_strength,
        defense = M_defense,
        hp = M_hp,
        dexterity = M_dexterity
    } = maps:get(BornMonth, BornTypeInfoMap),

    {PlayerName, CharacterDescription, SelfDescription} =
        case Gender of
            male ->
                {{nls, npc_little_boy}, {nls, npc_little_boy_desc}, {nls, self_npc_little_boy_desc}};
            female ->
                {{nls, npc_little_girl}, {nls, npc_little_girl_desc}, {nls, self_npc_little_girl_desc}}
        end,

    FinalPlayerProfile = PlayerProfile#player_profile{
        register_time = elib:timestamp(),
        scene = ?BORN_SCENE,
        name = PlayerName,
        description = CharacterDescription,
        self_description = SelfDescription,
        battle_status = #battle_status{
            'Strength' = M_strength,
            'M_Strength' = M_strength,
            'Defense' = M_defense,
            'M_defense' = M_defense,
            'Hp' = M_hp,
            'M_hp' = M_hp,
            'Dexterity' = M_dexterity,
            'M_dexterity' = M_dexterity
        }
    },

    UpdatedState = State#state{self = FinalPlayerProfile},

    ok = login_server:registration_done(FinalPlayerProfile),
    ok = login_server:login(DispatcherPid, PlayerUid),
    {stop, normal, UpdatedState};
input_confirmation(
    cast,
    {Answer, DispatcherPid},
    #state{
        self = #player_profile{
            lang = Lang,
            uid = Uid
        },
        born_type_info_map = BornTypeInfoMap
    }
) when Answer == <<"n">> orelse Answer == <<"no">> ->
    nls_server:response_content([{nls, please_input_id}], Lang, DispatcherPid),
    {
        next_state,
        input_id,
        #state{
            self = #player_profile{
                uid = Uid,
                lang = Lang
            },
            born_type_info_map = BornTypeInfoMap
        }
    };
input_confirmation(
    cast,
    {Other, DispatcherPid},
    #state{
        self = #player_profile{
            lang = Lang
        },
        summary_content = SummaryContent
    } = State
) ->
    nls_server:response_content(lists:flatten([{nls, invalid_command}, Other, <<"\n\n">>, SummaryContent]), Lang, DispatcherPid),
    {next_state, input_confirmation, State};
input_confirmation(
    {call, From},
    current_player_profile,
    #state{
        self = CurrentPlayerProfile
    }
) ->
    {keep_state_and_data, {reply, From, CurrentPlayerProfile}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored. If the gen_statem is terminated
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
    gen_statem:format_status(Opt, StatusData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the callback mode to gen_statem
%%
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

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
    Month :: player_statem:born_month().
validate_month(MonthBin) ->
    try
        case binary_to_integer(MonthBin) of
            Month when Month >= 1, Month =< 12 ->
                {ok, Month};
            _InvalidMonth ->
                {false, MonthBin}
        end
    catch
        _ErrorType:_Reason:_Stacktrace ->
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
    [_RecordName | RecordValues] = tuple_to_list(PlayerProfile),
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
gen_summary_content([_FieldName | RestFieldNames], [_Value | RestValues], AccSummary) ->
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
gen_summary_convert_value(_Key, Value) when is_integer(Value) ->
    integer_to_binary(Value);
gen_summary_convert_value(_Key, Value) when is_atom(Value) ->
    {nls, Value}.
%%gen_summary_convert_value(_Key, Value) ->
%%    Value.