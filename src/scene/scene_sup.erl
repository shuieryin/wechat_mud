%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 九月 2015 下午10:42
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

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
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
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildList = gen_child_list(),
    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec gen_child_list() -> ChildList when
    ChildList :: {WorkerName, {scene_fsm, start_link, []}, Restart, Shutdown, Type, [scene_fsm]},
    Restart :: supervisor:restart(),
    Shutdown :: supervisor:shutdown(),
    Type :: supervisor:worker(),
    WorkerName :: atom().
gen_child_list() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, FileNameList} = file:list_dir(?SCENE_PATH),
    gen_child_list(FileNameList, [], Restart, Shutdown, Type).

-spec gen_child_list(FileNameList, AccSceneList, Restart, Shutdown, Type) -> [term()] when
    FileNameList :: filename:filename(),
    AccSceneList :: [term()],
    Restart :: atom(),
    Shutdown :: pos_integer(),
    Type :: atom().
gen_child_list([], AccSceneList, _, _, _) ->
    AccSceneList;
gen_child_list([FileName | Tail], AccSceneList, Restart, Shutdown, Type) ->
    case filename:extension(FileName) == ?FILE_EXTENSION of
        true ->
            {ok, SceneFile} = file:open(filename:join(?SCENE_PATH, FileName), [read]),
            {ok, CurScenesList} = ecsv:process_csv_file_with(SceneFile, fun gen_scenes/2, {0, Restart, Shutdown, Type}),
            ok = file:close(SceneFile),

            gen_child_list(Tail, CurScenesList ++ AccSceneList, Restart, Shutdown, Type);
        _ ->
            gen_child_list(Tail, AccSceneList, Restart, Shutdown, Type)
    end.
%%--------------------------------------------------------------------
%% @doc
%% Read line from csv file
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_scenes(NewLineData, State) -> FinalSceneList when
    NewLineData :: {newline, NewLine} | {eof},
    NewLine :: [term()],
    State :: {Counter, KeysMap, ValuesMap} | {0, ValuesMap},
    Counter :: non_neg_integer(),
    KeysMap :: map(),
    ValuesMap :: map(),
    FinalSceneList :: [term()].
gen_scenes({newline, NewLine}, {Counter, Restart, Shutdown, Type, KeysMap, SceneList}) ->
    CurScene = gen_scene(NewLine, KeysMap, #{}, 0, Restart, Shutdown, Type),
    {Counter + 1, Restart, Shutdown, Type, KeysMap, [CurScene] ++ SceneList};
gen_scenes({newline, NewLine}, {0, Restart, Shutdown, Type}) ->
    KeysMap = gen_keysmap(NewLine, #{}, 0),
    {1, Restart, Shutdown, Type, KeysMap, []};
gen_scenes({eof}, {_, _, _, _, _, FinalSceneList}) ->
    FinalSceneList.

%%--------------------------------------------------------------------
%% @doc
%% generate keys map from first line
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_keysmap(NewLine, KeysMap, Pos) -> FinalKeysMap when
    NewLine :: [term()],
    KeysMap :: map(),
    Pos :: non_neg_integer(),
    FinalKeysMap :: map().
gen_keysmap([], KeysMap, _) ->
    KeysMap;
gen_keysmap([RawKey | Tail], KeysMap, Pos) ->
    Key = list_to_atom(RawKey),
    gen_keysmap(Tail, KeysMap#{Pos => Key}, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% generate values map from rest of lines
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_scene(NewLine, KeysMap, ValuesMap, Pos, Restart, Shutdown, Type) -> FinalValuesMap when
    NewLine :: [term()],
    KeysMap :: map(),
    ValuesMap :: map(),
    Pos :: non_neg_integer(),
    Restart :: atom(),
    Shutdown :: pos_integer(),
    Type :: atom(),
    FinalValuesMap :: map().
gen_scene([], _, ValuesMap, _, Restart, Shutdown, Type) ->
    Id = maps:get(id, ValuesMap),
    {Id, {scene_fsm, start_link, [{init, ValuesMap}]}, Restart, Shutdown, Type, [scene_fsm]};
gen_scene([[] | Tail], KeysMap, ValuesMap, Pos, Restart, Shutdown, Type) ->
    gen_scene(Tail, KeysMap, ValuesMap, Pos + 1, Restart, Shutdown, Type);
gen_scene([RawValue | Tail], KeysMap, ValuesMap, Pos, Restart, Shutdown, Type) ->
    Key = maps:get(Pos, KeysMap),
    Value = case Key of
                id ->
                    list_to_atom(RawValue);
                exits ->
                    {ok, Tokens, _} = erl_scan:string(RawValue),
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    Term;
                title ->
                    list_to_atom(RawValue);
                desc ->
                    list_to_atom(RawValue);
                nls_server ->
                    list_to_atom(RawValue);
                _ ->
                    undefined
            end,
    UpdatedValuesMap = case Value of
                           undefined ->
                               ValuesMap;
                           _ ->
                               ValuesMap#{Key => Value}
                       end,
    gen_scene(Tail, KeysMap, UpdatedValuesMap, Pos + 1, Restart, Shutdown, Type).