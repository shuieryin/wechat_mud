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
-export([start_link/0,
    new_npcs/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-define(SERVER, ?MODULE).
-define(NPC_FSM_IDS_MAP, nps_fsm_ids).

-type uuid() :: atom().
-type npc_type() :: dog | little_boy.
-type npc_spec() :: {npc_type(), Amount :: pos_integer()}.
-type npc_born_info() :: #{npc_id => atom(), name_nls_key => atom(), description_nls_key => atom(), attack => integer(), defence => integer(), hp => integer(), dexterity => integer()}.

-export_type([npc_type/0,
    npc_spec/0,
    npc_born_info/0]).

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
%% Starts npc fsm as requirements of NpcSpecs.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npcs(NpcsSpec) -> SceneNpcFsmList when
    NpcsSpec :: [npc_spec()],
    SceneNpcFsmList :: [scene_fsm:npc_fsm()].
new_npcs([]) ->
    [];
new_npcs(NpcsSpec) ->
    {SceneNpcFsmList, NpcFsmMap} = traverse_npcspec(NpcsSpec),
    gen_server:cast(?MODULE, {new_npcs, NpcFsmMap}),
    SceneNpcFsmList.

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
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: term(),
    State :: map(),
    Reason :: term().
init([]) ->
    {ok, #{?NPC_FSM_IDS_MAP => #{}}}.

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

    Request :: term(),
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
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

    Request :: {new_npcs, NewNpcFsmIdsMap},
    NewNpcFsmIdsMap :: #{uuid() => uuid()},
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_cast({new_npcs, NewNpcFsmIdsMap}, #{?NPC_FSM_IDS_MAP := NpcFsmIdsMap} = State) ->
    UpdatedNpcFsmIdsMap = maps:merge(NpcFsmIdsMap, NewNpcFsmIdsMap),
    {noreply, State#{?NPC_FSM_IDS_MAP := UpdatedNpcFsmIdsMap}}.


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
%% Traverses Npcs spec and starts certain amounts of npc fsms by
%% specific npc types.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_npcspec(NpcsSpec) -> {SceneNpcFsmList, NpcFsmMap} when
    NpcsSpec :: [npc_spec()],
    SceneNpcFsmList :: [scene_fsm:npc_fsm()],
    NpcFsmMap :: #{uuid() => uuid()}.
traverse_npcspec(NpcsSpec) ->
    traverse_npcspec(NpcsSpec, [], #{}).

%%--------------------------------------------------------------------
%% @doc
%% Traverses Npcs spec and starts certain amounts of npc fsms by
%% specific npc types.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_npcspec(NpcsSpec, AccNpcFsmList, AccNpcFsmMap) -> {NpcFsmList, NpcFsmMap} when
    NpcsSpec :: [npc_spec()],
    AccNpcFsmList :: [scene_fsm:npc_fsm()],
    AccNpcFsmMap :: #{uuid() => uuid()},
    NpcFsmList :: AccNpcFsmList,
    NpcFsmMap :: AccNpcFsmMap.
traverse_npcspec([], AccNpcFsmList, AccNpcFsmMap) ->
    {AccNpcFsmList, AccNpcFsmMap};
traverse_npcspec([{NpcType, Amount} | Tail], AccNpcFsmList, AccNpcFsmMap) ->
    NpcBornProfile = common_server:get_runtime_data([npcs, NpcType]),
    {UpdatedAccNpcFsmList, UpdatedAccNpcFsmMap} = new_npc(Amount, NpcBornProfile, AccNpcFsmList, AccNpcFsmMap),
    traverse_npcspec(Tail, UpdatedAccNpcFsmList, UpdatedAccNpcFsmMap).

%%--------------------------------------------------------------------
%% @doc
%% Starts certain amounts of npc fsm with the same npc type.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npc(Amount, NpcBornProfile, AccNpcFsmList, AccOverallNpcFsmMap) -> {NpcFsmList, OverallNpcFsmMap} when
    Amount :: pos_integer(),
    NpcBornProfile :: npc_born_info(),
    AccNpcFsmList :: [scene_fsm:npc_fsm()],
    AccOverallNpcFsmMap :: #{uuid() => uuid()},
    NpcFsmList :: AccNpcFsmList,
    OverallNpcFsmMap :: AccOverallNpcFsmMap.
new_npc(0, _, AccNpcFsmList, AccOverallNpcFsmMap) ->
    {AccNpcFsmList, AccOverallNpcFsmMap};
new_npc(Amount, #{npc_id := NpcType, name_nls_key := NameNlsKey} = NpcBornProfile, AccNpcFsmList, AccOverallNpcFsmMap) ->
    NpcUuid = list_to_atom(uuid:uuid_to_string(uuid:get_v4())),
    NpcProfile = NpcBornProfile#{npc_uuid => NpcUuid},
    npc_fsm_sup:add_child(NpcProfile),
    new_npc(Amount - 1, NpcBornProfile, [{npc, NpcUuid, NpcType, NameNlsKey} | AccNpcFsmList], AccOverallNpcFsmMap#{NpcUuid => NpcUuid}).