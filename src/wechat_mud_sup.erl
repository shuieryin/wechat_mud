%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Wechat mud root supervisor.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(wechat_mud_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/1
]).

%% Supervisor callbacks
-export([
    init/1
]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(StartArgs) -> supervisor:startlink_ret() when
    StartArgs :: [term()].
start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

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
-spec init(StartArgs) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore when
    StartArgs :: [term()].
init([AppName]) ->
    InfoServerName = list_to_atom(AppName ++ "_information_server"),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {
        SupFlags,
        [
            {redis_client_server,
                {redis_client_server, start_link, []},
                permanent,
                10000,
                worker,
                [redis_client_server]
            },

            {common_server,
                {common_server, start_link, []},
                permanent,
                10000,
                worker,
                [common_server]
            },

            {nls_server,
                {nls_server, start_link, []},
                permanent,
                10000,
                worker,
                [nls_server]
            },

            {login_server,
                {login_server, start_link, []},
                permanent,
                10000,
                worker,
                [login_server]
            },

            {npc_root_sup,
                {npc_root_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [npc_root_sup]
            },

            {scene_root_sup,
                {scene_root_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [scene_root_sup]
            },

            {register_statem_sup,
                {register_statem_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [register_statem_sup]
            },

            {player_statem_sup,
                {player_statem_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [player_statem_sup]
            },

            {InfoServerName,
                {information_server, start_link, [{cm, q, []}, InfoServerName]},
                permanent,
                10000,
                worker,
                [InfoServerName]
            }
        ]
    }}.