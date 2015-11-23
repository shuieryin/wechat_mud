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

-type npc_fsm_id() :: atom(). % generic atom
-type npc_type() :: dog | little_boy.
-type npc_amount() :: pos_integer(). % generic integer
-type npc_spec() :: {npc_type(), npc_amount()}.
-type npc_fsm() :: #{npc_fsm_id() => npc_fsm_id()}.

-include("../data_type/npc_born_info.hrl").

-record(state, {
    npc_fsms_map :: npc_fsm()
}).

-export_type([npc_type/0,
    npc_spec/0]).

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
%% Starts npc fsm as requirements of NpcSpecs.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npcs(NpcsSpec) -> SceneNpcFsmList when
    NpcsSpec :: [npc_spec()] | undefined,
    SceneNpcFsmList :: [#simple_npc_fsm{}].
new_npcs(undefined) ->
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
-spec init([]) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    {ok, #state{npc_fsms_map = #{}}}.

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

    Request :: {new_npcs, NewNpcFsmsMap},
    NewNpcFsmsMap :: npc_fsm(),
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast({new_npcs, NewNpcFsmsMap}, #state{npc_fsms_map = NpcFsmsMap} = State) ->
    UpdatedNpcFsmsMap = maps:merge(NpcFsmsMap, NewNpcFsmsMap),
    {noreply, State#state{npc_fsms_map = UpdatedNpcFsmsMap}}.


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
-spec traverse_npcspec(NpcsSpec) -> {SceneNpcFsmList, NpcFsmMap} when
    NpcsSpec :: [npc_spec()],
    SceneNpcFsmList :: [#simple_npc_fsm{}],
    NpcFsmMap :: #{npc_fsm_id() => npc_fsm_id()}.
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
-spec traverse_npcspec(NpcsSpec, AccNpcFsmList, AccNpcFsmMap) -> {NpcFsmList, NpcFsmMap} when
    NpcsSpec :: [npc_spec()],
    AccNpcFsmList :: [#simple_npc_fsm{}],
    AccNpcFsmMap :: #{npc_fsm_id() => npc_fsm_id()},
    NpcFsmList :: AccNpcFsmList,
    NpcFsmMap :: AccNpcFsmMap.
traverse_npcspec([], AccNpcFsmList, AccNpcFsmMap) ->
    {AccNpcFsmList, AccNpcFsmMap};
traverse_npcspec([{NpcType, Amount} | Tail], AccNpcFsmList, AccNpcFsmMap) ->
    NpcBornProfile = common_server:get_runtime_data([npc_born_info, NpcType]),
    {UpdatedAccNpcFsmList, UpdatedAccNpcFsmMap} = new_npc(Amount, NpcBornProfile, AccNpcFsmList, AccNpcFsmMap),
    traverse_npcspec(Tail, UpdatedAccNpcFsmList, UpdatedAccNpcFsmMap).

%%--------------------------------------------------------------------
%% @doc
%% Starts certain amounts of npc fsm with the same npc type.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_npc(Amount, NpcBornProfile, AccNpcFsmList, AccOverallNpcFsmMap) -> {NpcFsmList, OverallNpcFsmMap} when
    Amount :: npc_amount(),
    NpcBornProfile :: #npc_born_info{},
    AccNpcFsmList :: [#simple_npc_fsm{}],
    AccOverallNpcFsmMap :: #{npc_fsm_id() => npc_fsm_id()},
    NpcFsmList :: AccNpcFsmList,
    OverallNpcFsmMap :: AccOverallNpcFsmMap.
new_npc(0, _, AccNpcFsmList, AccOverallNpcFsmMap) ->
    {AccNpcFsmList, AccOverallNpcFsmMap};
new_npc(Amount, #npc_born_info{npc_id = NpcType, name_nls_key = NameNlsKey} = NpcBornProfile, AccNpcFsmList, AccOverallNpcFsmMap) ->
    NpcFsmId = list_to_atom(uuid:uuid_to_string(uuid:get_v4())),
    NpcProfile = NpcBornProfile#npc_born_info{npc_fsm_id = NpcFsmId},
    npc_fsm_sup:add_child(NpcProfile),
    new_npc(Amount - 1, NpcBornProfile, [#simple_npc_fsm{npc_fsm_id = NpcFsmId, npc_type = NpcType, npc_name_nls_key = NameNlsKey} | AccNpcFsmList], AccOverallNpcFsmMap#{NpcFsmId => NpcFsmId}).