%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Scene supervisor. This supervisor starts up scene worker per row
%%% of scene csv file under priv/scene by scene_fsm_sup.erl which the number
%%% of children configs equals to the number of rows in scene csv file.
%%%
%%% @end
%%% Created : 14. Sep 2015 10:42 PM
%%%-------------------------------------------------------------------
-module(scene_fsm_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start/0,
    stop/0
]).

%% Supervisor callbacks
-export([init/1]).

-include("../data_type/scene_info.hrl").

-define(SERVER, ?MODULE).

%% Unique scene file extension
-define(FILE_EXTENSION, ".csv").

-type scene_child() :: {scene_fsm:scene_name(), {scene_fsm, start_link, [#scene_info{}]}, supervisor:restart(), supervisor:shutdown(), supervisor:worker(), [scene_fsm]}.

-export_type([
    scene_child/0
]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> supervisor:startlink_ret().
start() ->
    gen_server:start({local, ?SERVER}, supervisor, {{local, ?SERVER}, ?MODULE, []}, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> true.
stop() ->
    exit(whereis(?SERVER), normal).

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

    ChildList = gen_scene_child_list(),
    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates scene worker configs per row of scene csv file under
%% priv/scene by scene_fsm_sup.erl which the number of children configs
%% equals to the number of rows in scene csv file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_scene_child_list() -> SceneChildList when
    SceneChildList :: [scene_child()].
gen_scene_child_list() ->
    SceneSpecsMap = scene_manager:scene_specs_map(),
    maps:values(SceneSpecsMap).