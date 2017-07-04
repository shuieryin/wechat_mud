%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2015 7:09 PM
%%%-------------------------------------------------------------------
-module(npc_statem).
-author("shuieryin").

-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    simple_npc/1,
    npc_state/1,
    non_battle/3
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    format_status/2,
    callback_mode/0
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
%% Creates a gen_statem process which calls Module:init/1 to
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
    gen_statem:start_link({local, NpcUid}, ?MODULE, NpcProfile, []).

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
    gen_statem:call(NpcUid, npc_state).

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
        npc_id = NpcId,
        npc_uid = NpcUid,
        strength = M_strength,
        defense = M_defense,
        hp = M_hp,
        dexterity = M_dexterity,
        ask_n_answers = AskNAnswers
    } = NpcProfile,
    io:format("~ninitializing npc [~p] with uid [~p]...", [NpcId, NpcUid]),

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

    io:format("initialized~n"),
    {ok, non_battle, State}.

%%--------------------------------------------------------------------
%% @doc
%% Refer to below functions for details.
%%
%% @see look.
%%
%% @end
%%--------------------------------------------------------------------
-spec non_battle(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {execute_command, CommandContext} |
    npc_state,

    CommandContext :: #command_context{},

    State :: #npc_state{},
    Action :: gen_statem:reply_action() | {reply, From, Reply},
    From :: gen_statem:from(),

    Reply :: term(), % generic term

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}}.
non_battle(
    cast,
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, NextStateName, UpdatedData} = CommandModule:CommandFunc(CommandContext, State, non_battle),
    {next_state, NextStateName, UpdatedData};
non_battle({call, From}, npc_state, NpcData) ->
    {keep_state_and_data, {reply, From, NpcData}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
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
%%% Internal functions (N/A)
%%%===================================================================
