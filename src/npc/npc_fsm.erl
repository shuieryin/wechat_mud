%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2015 7:09 PM
%%%-------------------------------------------------------------------
-module(npc_fsm).
-author("shuieryin").

-behaviour(gen_fsm).

%% API
-export([
    start_link/1,
    non_battle/2
]).

%% gen_fsm callbacks
-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    format_status/2,
    simple_npc/1,
    npc_state/1
]).

-define(SERVER, ?MODULE).

-include("../data_type/npc_profile.hrl").
-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

-type npc_uid() :: atom(). % generic atom
-type npc_id() :: binary(). % generic binary
-type npc_name() :: nls_server:nls_object().

-type npc_state_name() :: non_battle.

-export_type([
    npc_uid/0,
    npc_id/0,
    npc_name/0,
    npc_state_name/0
]).

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
-spec start_link(NpcProfile) -> gen:start_ret() when
    NpcProfile :: #npc_profile{}.
start_link(
    #npc_profile{
        npc_uid = NpcUid
    } = NpcProfile
) ->
    gen_fsm:start_link({local, NpcUid}, ?MODULE, NpcProfile, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns simple npc record.
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_npc(NpcProfile) -> SimpleNpc when
    NpcProfile :: #npc_profile{},
    SimpleNpc :: #simple_npc{}.
simple_npc(
    #npc_profile{
        npc_uid = NpcUid,
        npc_id = NpcId,
        npc_name = NpcName
    }
) ->
    #simple_npc{
        npc_uid = NpcUid,
        npc_id = NpcId,
        npc_name = NpcName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Returns full npc state.
%%
%% @end
%%--------------------------------------------------------------------
-spec npc_state(npc_uid()) -> #npc_state{}.
npc_state(NpcUid) ->
    gen_fsm:sync_send_all_state_event(NpcUid, npc_state).

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
-spec init(NpcProfile) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    StateName :: npc_state_name(),
    StateData :: #npc_state{},
    Reason :: term(), % generic term
    NpcProfile :: #npc_profile{}.
init(NpcProfile) ->
    #npc_profile{
        strength = M_strength,
        defense = M_defense,
        hp = M_hp,
        dexterity = M_dexterity,
        ask_n_answers = AskNAnswers
    } = NpcProfile,

    NpcContext = lists:foldl(
        fun(#ask_n_answer{affair_mod = AffairMod}, AccNpcContext) ->
            UpdatedAccNpcContext = AffairMod:init(NpcProfile, AccNpcContext),
            UpdatedAccNpcContext
        end, #{}, AskNAnswers),

    State = #npc_state{
        self = NpcProfile,
        battle_status = #battle_status{
            'Strength' = M_strength,
            'M_Strength' = M_strength,
            'Defense' = M_defense,
            'M_defense' = M_defense,
            'Hp' = M_hp,
            'M_hp' = M_hp,
            'Dexterity' = M_dexterity,
            'M_dexterity' = M_dexterity
        },
        npc_context = NpcContext
    },
    {ok, non_battle, State}.

%%--------------------------------------------------------------------
%% @doc
%% Refer to below functions for details.
%%
%% @see look.
%%
%% @end
%%--------------------------------------------------------------------
-spec non_battle(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event :: {execute_command, CommandContext},

    CommandContext :: #command_context{},

    State :: #npc_state{},
    NextStateName :: npc_state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
non_battle(
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, non_battle),
    {next_state, NextStateName, UpdatedState}.

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

    Event :: term(), % generic term
    StateName :: npc_state_name(),
    StateData :: #npc_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
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

    Event :: npc_state, % generic term
    Reply :: #npc_state{},

    From :: {pid(), Tag :: term()}, % generic term
    StateName :: npc_state_name(),
    StateData :: #npc_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_sync_event(npc_state, _From, StateName, NpcState) ->
    {reply, NpcState, StateName, NpcState}.

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
    StateName :: npc_state_name(),
    StateData :: #npc_state{},
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
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, StateName, StateData) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(), % generic term
    StateName :: npc_state_name(),
    StateData :: #npc_state{}.
terminate(_Reason, _StateName, _State) ->
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
    StateName :: npc_state_name(),
    StateData :: #npc_state{},
    Extra :: term(), % generic term
    NextStateName :: StateName,
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
    State :: #npc_state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_fsm:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
