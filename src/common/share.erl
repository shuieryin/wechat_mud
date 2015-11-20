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
-module(share).
-author("Shuieryin").

%% API
-export([is_module_exist/1,
    type_of/1,
    timestamp/0,
    hot_code_replace/1,
    index_of/2,
    until_process_terminated/2,
    gen_doc/0,
    increase_vsn/3,
    hot_code_upgrade/0,
    quit/0,
    first_to_lower/1,
    remove_last_newline/1,
    random_from_list/1,
    observer/0,
    binary_join/2]).

-type valid_type() :: atom | bitstring | boolean | float | function | integer | list | pid | port | reference | tuple.

-define(OBSERVER_NODE, 'collin@192.168.1.110').

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
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_boolean(X) -> boolean;
type_of(X) when is_function(X) -> function;
type_of(X) when is_pid(X) -> pid;
type_of(X) when is_port(X) -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X) -> atom;
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
-spec until_process_terminated(PidOrName, DetectPeriodInMilli) -> ok when
    PidOrName :: pid() | erlang:registered_name(),
    DetectPeriodInMilli :: pos_integer(). % generic integer
until_process_terminated(PidOrName, DetectPeriodInMilli) ->
    IsTerminatedFun = case is_pid(PidOrName) of
                          true ->
                              fun() -> process_info(PidOrName) end;
                          _ ->
                              fun() -> whereis(PidOrName) end
                      end,
    until_process_terminated(IsTerminatedFun(), IsTerminatedFun, DetectPeriodInMilli).
until_process_terminated(undefined, _, _) ->
    ok;
until_process_terminated(_, IsTerminatedFun, DetectPeriodInMilli) ->
    timer:sleep(DetectPeriodInMilli),
    IsProcessTerminated = IsTerminatedFun(),
    until_process_terminated(IsProcessTerminated, IsTerminatedFun, IsTerminatedFun).

%%--------------------------------------------------------------------
%% @doc
%% This function generates edoc in html format.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_doc() -> ok.
gen_doc() ->
    edoc:application(wechat_mud, [{dir, "doc/edoc"}]).

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
%% Terminate redis-server and the erlang server.
%%
%% @end
%%--------------------------------------------------------------------
-spec quit() -> no_return().
quit() ->
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
    Element :: term(), % generic term
    SrcList :: [Element].
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
            _ ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]).