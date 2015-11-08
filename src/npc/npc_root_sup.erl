%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% This is npc root supervisor which starts up npc_fsm_manager and npc_fsm_sup.
%%%
%%% @end
%%% Created : 06. Nov 2015 4:38 PM
%%%-------------------------------------------------------------------
-module(npc_root_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
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
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {
        SupFlags,
        [
            {npc_fsm_manager,
                {npc_fsm_manager, start_link, []},
                permanent,
                10000,
                worker,
                [npc_fsm_manager]
            },

            {npc_fsm_sup,
                {npc_fsm_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [npc_fsm_sup]
            }
        ]
    }}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================
