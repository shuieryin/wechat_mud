-module(wechat_mud_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    [{AppName, _, _, _}] = release_handler:which_releases(permanent),
    erlang:set_cookie(node(), list_to_atom(AppName)),

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

            {nls_sup,
                {nls_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [nls_sup]
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

            {scene_sup,
                {scene_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [scene_sup]
            },

            {register_fsm_sup,
                {register_fsm_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [register_fsm_sup]
            },

            {player_fsm_sup,
                {player_fsm_sup, start_link, []},
                permanent,
                10000,
                supervisor,
                [player_fsm_sup]
            }
        ]
    }}.

