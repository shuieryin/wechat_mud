%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% This is npc fsm manager, it manages npcs all over the server.
%%% all over the server.
%%%
%%% @end
%%% Created : 06. Nov 2015 4:47 PM
%%%-------------------------------------------------------------------
-module(npc_fsm_manager).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    new_npcs/1,
    stop/0
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

-type npc_amount() :: pos_integer(). % generic integer
-type npc_spec() :: {npc_fsm:npc_id(), npc_amount()}.
-type npcs_map() :: #{npc_fsm:npc_uid() => npc_fsm:npc_uid()}.

-include("../data_type/npc_profile.hrl").

-record(state, {
    npc_fsms_map :: npcs_map(),
    npc_fsm_ids_bank :: gb_sets:set(npc_fsm:npc_uid())
}).

-export_type([
    npc_spec/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop server.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts npc fsm as requirements of NpcSpecs.
%% TODO: mark down npc uuids for reusable purpose, this is to prevent from growing memory size of atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npcs(NpcsSpec) -> SceneNpcsList when
    NpcsSpec :: [npc_spec()] | undefined,
    SceneNpcsList :: [#simple_npc{}].
new_npcs(undefined) ->
    [];
new_npcs(NpcsSpec) ->
    {SceneNpcsList, NpcsMap} = traverse_npcspec(NpcsSpec),
    gen_server:cast(?MODULE, {new_npcs, NpcsMap}),
    SceneNpcsList.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server by setting npc fsm ids to emtpy list.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
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
    {
        ok,
        #state{
            npc_fsms_map = #{},
            npc_fsm_ids_bank = gb_sets:new()
        }
    }.

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

    Request :: term(), % generic term
    Reply :: ok,

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

    Request :: {new_npcs, NewNpcsMap} | stop,

    NewNpcsMap :: npcs_map(),

    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast(
    {new_npcs, NewNpcsMap},
    #state{
        npc_fsms_map = NpcsMap
    } = State
) ->
    UpdatedNpcsMap = maps:merge(NpcsMap, NewNpcsMap),
    {noreply, State#state{npc_fsms_map = UpdatedNpcsMap}};
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
%% Traverses Npcs spec and starts certain amounts of npc fsms by
%% specific npc types.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_npcspec(NpcsSpec) -> {SceneNpcsList, NpcsMap} when
    NpcsSpec :: [npc_spec()],
    SceneNpcsList :: [#simple_npc{}],
    NpcsMap :: npcs_map().
traverse_npcspec(NpcsSpec) ->
    io:format("NpcsSpec:~p~n", [NpcsSpec]),
    traverse_npcspec(NpcsSpec, [], #{}).

%%--------------------------------------------------------------------
%% @doc
%% Traverses Npcs spec and starts certain amounts of npc fsms by
%% specific npc types.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_npcspec(NpcsSpec, AccNpcsList, AccNpcsMap) -> {NpcsList, NpcsMap} when
    NpcsSpec :: [npc_spec()],
    AccNpcsList :: [#simple_npc{}],
    AccNpcsMap :: npcs_map(),
    NpcsList :: AccNpcsList,
    NpcsMap :: AccNpcsMap.
traverse_npcspec([], AccNpcsList, AccNpcsMap) ->
    {AccNpcsList, AccNpcsMap};
traverse_npcspec([{NpcId, Amount} | Tail], AccNpcsList, AccNpcsMap) ->
    NpcBornProfile = common_server:get_runtime_data(npc_profile, NpcId),
    {UpdatedAccNpcsList, UpdatedAccNpcsMap} = new_npc(Amount, NpcBornProfile, AccNpcsList, AccNpcsMap),
    traverse_npcspec(Tail, UpdatedAccNpcsList, UpdatedAccNpcsMap).

%%--------------------------------------------------------------------
%% @doc
%% Starts certain amounts of npc fsm with the same npc type.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npc(Amount, NpcBornProfile, AccNpcsList, AccOverallNpcsMap) -> {NpcsList, OverallNpcsMap} when
    Amount :: npc_amount(),
    NpcBornProfile :: #npc_profile{},
    AccNpcsList :: [#simple_npc{}],
    AccOverallNpcsMap :: npcs_map(),
    NpcsList :: AccNpcsList,
    OverallNpcsMap :: AccOverallNpcsMap.
new_npc(0, _, AccNpcsList, AccOverallNpcsMap) ->
    {AccNpcsList, AccOverallNpcsMap};
new_npc(Amount, NpcBornProfile, AccNpcsList, AccOverallNpcsMap) ->
    NpcUid = cm:uuid(),
    NpcProfile = NpcBornProfile#npc_profile{
        npc_uid = NpcUid
    },
    npc_fsm_sup:add_child(NpcProfile),
    new_npc(
        Amount - 1,
        NpcBornProfile,
        [npc_fsm:simple_npc(NpcProfile) | AccNpcsList],
        AccOverallNpcsMap#{
            NpcUid => NpcUid
        }
    ).