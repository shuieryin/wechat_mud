%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Common API module. This module provides APIs that handles generic handlings.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(cm).
-author("Shuieryin").

%% API
-export([
    is_module_exist/1,
    type_of/1,
    timestamp/0,
    hot_code_replace/1,
    index_of/2,
    until_process_terminated/1,
    increase_vsn/3,
    hot_code_upgrade/0,
    q/0,
    first_to_lower/1,
    remove_last_newline/1,
    random_from_list/1,
    observer/0,
    binary_join/2,
    type_values/2,
    uuid/0,
    rr/2,
    parse_target_id/1,
    target/2,
    general_target/2
]).

-type valid_type() :: atom | binary | bitstring | boolean | float | function | integer | list | pid | port | reference | tuple | map.

-define(OBSERVER_NODE, 'macbook@192.168.1.110').

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../data_type/scene_info.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check if module exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_module_exist(Module) -> boolean() when
    Module :: module().
is_module_exist(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _:_ ->
                    false
            end;

        false ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Detect value type.
%%
%% @end
%%--------------------------------------------------------------------
-spec type_of(X) -> Result when
    X :: term(), % generic term
    Result :: valid_type() | unknown.
type_of(X) when is_integer(X) -> integer;
type_of(X) when is_float(X) -> float;
type_of(X) when is_list(X) -> list;
type_of(X) when is_tuple(X) -> tuple;
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_boolean(X) -> boolean;
type_of(X) when is_function(X) -> function;
type_of(X) when is_pid(X) -> pid;
type_of(X) when is_port(X) -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X) -> atom;
type_of(X) when is_map(X) -> map;
type_of(_X) -> unknown.

%%--------------------------------------------------------------------
%% @doc
%% Return timestamp in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> Timestamp when
    Timestamp :: pos_integer(). % generic integer
timestamp() ->
    {Hour, Minute, _} = os:timestamp(),
    Hour * 1000000 + Minute.

%%--------------------------------------------------------------------
%% @doc
%% Hot code replace modules by "ModuleNameList".
%%
%% @end
%%--------------------------------------------------------------------
-spec hot_code_replace(ModuleNameList) -> Result when
    ModuleName :: module(),
    ModuleNameList :: [ModuleName],
    Result :: [code:load_ret()].
hot_code_replace(ModuleNameList) ->
    [begin code:purge(ModuleName), code:load_file(ModuleName) end || ModuleName <- ModuleNameList].

%%--------------------------------------------------------------------
%% @doc
%% Finds the element position from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec index_of(Item, List) -> Pos when
    List :: [term()], % generic term
    Item :: term(), % generic term
    Pos :: -1 | pos_integer(). % generic integer
index_of(Item, List) ->
    index_of(Item, List, 1).
index_of(_, [], _) ->
    -1;
index_of(Elem, [Elem | _], Pos) ->
    Pos;
index_of(Item, [_ | Tail], Pos) ->
    index_of(Item, Tail, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Checks if pid or register name process still exists in "DetectPeriodInMilli"
%% milliseconds and return ok until the target is terminated. Use this
%% function in extreme caution! Only when you are 100% sure that the
%% target process is going to be terminated otherwise this function never returns.
%%
%% @end
%%--------------------------------------------------------------------
-spec until_process_terminated(PidOrName) -> ok when
    PidOrName :: erlang:monitor_process_identifier().
until_process_terminated(PidOrName) ->
    if
        PidOrName /= undefined ->
            MonitorRef = monitor(process, PidOrName),
            receive
                {'DOWN', MonitorRef, process, _, _} ->
                    ok
            end;
        true ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function increases version number
%%
%% @end
%%--------------------------------------------------------------------
-spec increase_vsn(SourceVersion, VersionDepth, Increment) -> UpgradedVersion when
    SourceVersion :: string(),
    VersionDepth :: pos_integer(), % generic integer
    Increment :: pos_integer(), % generic integer
    UpgradedVersion :: SourceVersion.
increase_vsn(SourceVersion, VersionDepth, Increment) ->
    string:join(increase_vsn(string:tokens(SourceVersion, "."), VersionDepth, Increment, 1, []), ".").

%%--------------------------------------------------------------------
%% @doc
%% Hot code upgrade for hot fixes which only increase version number of the third depth.
%%
%% @end
%%--------------------------------------------------------------------
hot_code_upgrade() ->
    [{AppName, CurVersion, _, _}] = release_handler:which_releases(permanent),
    NewVersion = increase_vsn(CurVersion, 3, 1),
    os:cmd("make OLD_VER=" ++ CurVersion ++ " NEW_VER=" ++ NewVersion ++ " upgrade"),
    %MakeCommand = "erl -noshell -eval 'cd(\"/root/workspaces/wechat_mud\"),io:format(\"~ts~n\",[os:cmd(\"make OLD_VER=" ++ CurVersion ++ " NEW_VER=" ++ NewVersion ++ " upgrade\")]),init:stop()'",
    %os:cmd(MakeCommand),
    release_handler:unpack_release(AppName ++ "_" ++ NewVersion),
    release_handler:install_release(NewVersion),
    release_handler:make_permanent(NewVersion).

%%--------------------------------------------------------------------
%% @doc
%% Terminate redis-server and the erlang server properly by committing
%% all possible states.
%%
%% @end
%%--------------------------------------------------------------------
-spec q() -> no_return().
q() ->
    os:cmd("redis-cli shutdown"),
    init:stop().

%%--------------------------------------------------------------------
%% @doc
%% Lowercases the first letter of give string.
%%
%% @end
%%--------------------------------------------------------------------
-spec first_to_lower(SrcString) -> FirstLoweredString when
    SrcString :: string(),
    FirstLoweredString :: SrcString.
first_to_lower([First | Rest] = SrcString) when is_list(SrcString) ->
    FirstLowered = string:to_lower([First]),
    FirstLowered ++ Rest.

%%--------------------------------------------------------------------
%% @doc
%% Removes last new line binary in List.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_last_newline(SrcList) -> RetList when
    SrcList :: [nls_server:value()],
    RetList :: SrcList.
remove_last_newline(SrcList) ->
    case lists:reverse(SrcList) of
        [<<"\n">> | Rest] ->
            lists:reverse(Rest);
        _ ->
            SrcList
    end.

%%--------------------------------------------------------------------
%% @doc
%% Random pick element from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec random_from_list(SrcList) -> Element when
    Element :: term() | undefined, % generic term
    SrcList :: [Element].
random_from_list([]) ->
    undefined;
random_from_list(SrcList) ->
    ListSize = length(SrcList),
    RandomPos = random:uniform(ListSize),
    lists:nth(RandomPos, SrcList).

%%--------------------------------------------------------------------
%% @doc
%% Connect observer node.
%%
%% @end
%%--------------------------------------------------------------------
-spec observer() -> pong | pang.
observer() ->
    net_adm:ping(?OBSERVER_NODE).

%%--------------------------------------------------------------------
%% @doc
%% Binary join with separator.
%%
%% @end
%%--------------------------------------------------------------------
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(
        fun(A, B) ->
            if
                bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                true -> A
            end
        end,
        <<>>,
        List
    ).

%%--------------------------------------------------------------------
%% @doc
%% Get type values if split by "|".
%%
%% For example:
%%
%%          Given type in nls_server: -ty¢pe support_lang() :: zh | en.
%%
%%          [zh, en] = type_values(nls_server, support_lang).
%%
%% @end
%%--------------------------------------------------------------------
-spec type_values(ModuleName, TypeName) -> TypeValues when
    ModuleName :: module(),
    TypeName :: atom(), % generic atom
    TypeValues :: term(). % generic term
type_values(ModuleName, TypeName) ->
    ModulePath = module_src_path(ModuleName),
    {ok, AbstractCode} = dialyzer_utils:get_abstract_code_from_src(ModulePath),
    {ok, TypeDict} = dialyzer_utils:get_record_and_type_info(AbstractCode),

    TypeKey = {type, TypeName, 0},
    case dict:is_key(TypeKey, TypeDict) of
        true ->
            {{ModuleName, {ModulePath, _}, {type, _, _, TypeList}, []}, any}
                = dict:fetch({type, TypeName, 0}, TypeDict),
            [TypeAtom || {_, _, TypeAtom} <- TypeList];
        false ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Generate uuid in atom.
%% Caution! Be careful of this function. Improper use may lead to memory leak.
%%
%% @end
%%--------------------------------------------------------------------
-spec uuid() -> atom(). % generic atom
uuid() ->
    list_to_atom(uuid:uuid_to_string(uuid:get_v4())).

%%--------------------------------------------------------------------
%% @doc
%% Random float range. Input arguments must be integer.
%%
%% @end
%%--------------------------------------------------------------------
-spec rr(Start, End) -> float() when
    Start :: pos_integer(),
    End :: Start.
rr(Start, End) ->
    Value = if
                Start > End ->
                    Start;
                true ->
                    Start + random:uniform(End - Start)
            end,
    Value / 100.

%%--------------------------------------------------------------------
%% @doc
%% TargetArgs is converted from
%%        binary "little boy 2" to "TargetId=little_boy" and "Sequence=2".
%%        binary "shuieryin" to "TargetId=collin" and "Sequence=1".
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_target_id(TargetArgs) -> {ok, TargetId, Sequence} when
    TargetArgs :: binary(),
    TargetId :: player_fsm:id() | npc_fsm_manager:npc_type(),
    Sequence :: non_neg_integer().
parse_target_id(TargetArgs) ->
    [RawSequence | Rest] = lists:reverse(re:split(TargetArgs, <<" ">>)),
    {TargetId, Sequence} =
        case Rest of
            [] ->
                {RawSequence, 1};
            _ ->
                case re:run(RawSequence, "^[0-9]*$") of
                    {match, _} ->
                        {cm:binary_join(lists:reverse(Rest), <<"_">>), binary_to_integer(RawSequence)};
                    _ ->
                        {re:replace(TargetArgs, <<" ">>, <<"_">>, [global, {return, binary}]), 1}
                end
        end,
    {ok, TargetId, Sequence}.

%%--------------------------------------------------------------------
%% @doc
%% This function is an aggregate function for executing action on target.
%% gen_fsm only.
%%
%% @end
%%--------------------------------------------------------------------
-spec target(Uid, TargetContent) -> ok when
    Uid :: player_fsm:uid(),
    TargetContent :: #target_content{}.
target(Uid, #target_content{actions = [Action | _]} = TargetContent) ->
    gen_fsm:send_event(Uid, {Action, TargetContent}).

%%--------------------------------------------------------------------
%% @doc
%% This function is an aggregate function for executing action on target.
%% gen_fsm only.
%%
%% The difference between target/2 is that no need to specify simulation
%% function on the current state event but uses common event "general_target".
%%
%% @see target/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec general_target(Uid, TargetContent) -> ok when
    Uid :: player_fsm:uid(),
    TargetContent :: #target_content{}.
general_target(Uid, TargetContent) ->
    gen_fsm:send_event(Uid, {general_target, TargetContent}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This is sub function of increase_vsn/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec increase_vsn(SourceVersion, VersionDepth, Increment, CurDepth, AccVersion) -> UpgradedVersion when
    SourceVersion :: [string()],
    VersionDepth :: pos_integer(), % generic integer
    Increment :: pos_integer(), % generic integer
    CurDepth :: VersionDepth,
    AccVersion :: SourceVersion,
    UpgradedVersion :: SourceVersion.
increase_vsn([], _, _, _, AccVersion) ->
    lists:reverse(AccVersion);
increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    UpdatedVersionNum =
        case CurDepth =:= VersionDepth of
            true ->
                integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment);
            false ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]).

%%--------------------------------------------------------------------
%% @doc
%% Get source path by module name.
%%
%% @end
%%--------------------------------------------------------------------
-spec module_src_path(ModuleName) -> SrcPath when
    ModuleName :: module(),
    SrcPath :: file:filename().
module_src_path(ModuleName) ->
    get_module_src_path(ModuleName:module_info(compile)).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function of module_src_path/1.
%% @see module_src_path/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_module_src_path(SourceCompileInfo) -> SrcPath when
    SourceCompileInfo :: any() | beam_lib:compinfo_entry(),
    SrcPath :: string().
get_module_src_path([{source, SrcPath} | _]) ->
    SrcPath;
get_module_src_path([_ | Rest]) ->
    get_module_src_path(Rest).