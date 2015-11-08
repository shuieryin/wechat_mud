%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Scene supervisor. This supervisor starts up scene worker per row
%%% of scene csv file under priv/scene by scene_sup.erl which the number
%%% of children configs equals to the number of rows in scene csv file.
%%%
%%% @end
%%% Created : 14. Sep 2015 10:42 PM
%%%-------------------------------------------------------------------
-module(scene_sup).
-author("shuieryin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("scene.hrl").

-type scene_child() :: {atom(), {scene_fsm, start_link, [{init, map()}]}, supervisor:restart(), supervisor:shutdown(), supervisor:worker(), [scene_fsm]}.

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

    ChildList = gen_scene_child_list(),
    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates scene worker configs per row of scene csv file under
%% priv/scene by scene_sup.erl which the number of children configs
%% equals to the number of rows in scene csv file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_scene_child_list() -> SceneChildList when
    SceneChildList :: [scene_child()].
gen_scene_child_list() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChildFun = fun(SceneValuesMap) ->
        populate_scene_child(SceneValuesMap, Restart, Shutdown, Type)
    end,

    ScenesMap = csv_to_object:traverse_merge_files(?SCENE_NLS_PATH, ChildFun),
    maps:values(ScenesMap).

%%--------------------------------------------------------------------
%% @doc
%% Generates scene worker configs entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_scene_child(SceneInfo, Restart, Shutdown, Type) -> SceneChild when
    SceneInfo :: scene_fsm:scene_info(),
    Restart :: supervisor:restart(),
    Shutdown :: supervisor:shutdown(),
    Type :: supervisor:worker(),
    SceneChild :: scene_child().
populate_scene_child(#{id := Id} = SceneInfo, Restart, Shutdown, Type) ->
    {Id, {scene_fsm, start_link, [{init, SceneInfo}]}, Restart, Shutdown, Type, [scene_fsm]}.