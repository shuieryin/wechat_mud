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

    {ok, {
        {one_for_one, 5, 10},
        [
            {redis_client,
                {redis_client_server, start_link, []},
                permanent,
                10000,
                worker,
                [redis_client_server]
            },

            {common,
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

            {login,
                {login_server, start_link, []},
                permanent,
                10000,
                worker,
                [login_server]
            },

            {scene_sup,
                {scene_sup, start_link, []},
                permanent,
                10000,
                worker,
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

