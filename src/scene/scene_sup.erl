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
-export([
    start_link/0,
    stop/0
]).

%% Supervisor callbacks
-export([init/1]).

-include("../data_type/scene_info.hrl").

-define(SERVER, ?MODULE).

%% Unique scene file extension
-define(FILE_EXTENSION, ".csv").

-type scene_child() :: {scene_fsm:scene_name(), {scene_fsm, start_link, [#scene_info{}]}, supervisor:restart(), supervisor:shutdown(), supervisor:worker(), [scene_fsm]}.

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

    ChildFun =
        fun(SceneValues) ->
            populate_scene_child(SceneValues, Restart, Shutdown, Type)
        end,

    SceneNlsPath = filename:join(code:priv_dir(wechat_mud), "scene"),
    ScenesMap = csv_to_object:traverse_merge_files(SceneNlsPath, ChildFun),
    maps:values(ScenesMap).

%%--------------------------------------------------------------------
%% @doc
%% Generates scene worker configs entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_scene_child(SceneValues, Restart, Shutdown, Type) -> SceneChild when
    SceneValues :: [csv_to_object:value()],
    Restart :: supervisor:restart(),
    Shutdown :: supervisor:shutdown(),
    Type :: supervisor:worker(),
    SceneChild :: scene_child().
populate_scene_child([_CityName | SceneValues], Restart, Shutdown, Type) ->
    [Verify | _] = SceneValues,
    case Verify of
        undefined ->
            undefined;
        _ ->
            #scene_info{id = Id} = SceneInfo = list_to_tuple([scene_info | SceneValues]),
            {Id, {scene_fsm, start_link, [SceneInfo]}, Restart, Shutdown, Type, [scene_fsm]}
    end.