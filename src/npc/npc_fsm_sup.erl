%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Npc_fsm supervisor. This is a simple one for one supervisor which
%%% npc_fsm children dynamically are added to or removed from it.
%%%
%%% @end
%%% Created : 01. Nov 2015 6:12 PM
%%%-------------------------------------------------------------------
-module(npc_fsm_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    add_child/1,
    stop/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("../data_type/npc_born_info.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> supervisor:startchild_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    exit(whereis(?SERVER), normal).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec add_child(NpcProfile) -> supervisor:startchild_ret() when
    NpcProfile :: #npc_born_info{}.
add_child(NpcProfile) ->
    supervisor:start_child(?MODULE, [NpcProfile]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    ChildSpec = {npc_fsm, {npc_fsm, start_link, []}, Restart, Shutdown, Type, [npc_fsm]},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
