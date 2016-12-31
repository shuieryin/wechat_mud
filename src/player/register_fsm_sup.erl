%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Register_fsm supervisor. This is a simple one for one supervisor
%%% which register_fsm children dynamically are added to or removed from it.
%%%
%%% @end
%%% Created : 01. Nov 2015 6:12 PM
%%%-------------------------------------------------------------------
-module(register_fsm_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    add_child/3
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
%% Add register_fsm
%%
%% @end
%%--------------------------------------------------------------------
-spec add_child(DispatcherPid, Uid, BornTypeInfoMap) -> supervisor:startchild_ret() when
    DispatcherPid :: pid(),
    Uid :: player_statem:uid(),
    BornTypeInfoMap :: register_fsm:born_type_info_map().
add_child(DispatcherPid, Uid, BornTypeInfoMap) ->
    supervisor:start_child(?MODULE, [DispatcherPid, Uid, BornTypeInfoMap]).

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

    ChildSpec = {register_fsm, {register_fsm, start_link, []}, Restart, Shutdown, Type, [register_fsm]},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================