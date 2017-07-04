%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% This is npc root supervisor which starts up npc_statem_manager and npc_statem_sup.
%%%
%%% @end
%%% Created : 06. Nov 2015 4:38 PM
%%%-------------------------------------------------------------------
-module(npc_root_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([
    start_link/0
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
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {
        SupFlags,
        [
            {npc_statem_manager,
                {npc_statem_manager, start_link, []},
                permanent,
                10000,
                worker,
                [npc_statem_manager]
            },

            {npc_statem_sup,
                {npc_statem_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [npc_statem_sup]
            }
        ]
    }}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
