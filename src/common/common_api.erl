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
-module(common_api).
-author("Shuieryin").

%% API
-export([is_module_exists/1,
    type_of/1,
    timestamp/0,
    hot_code_replace/1,
    index_of/2,
    until_process_terminated/2,
    gen_doc/0]).

-type valid_type() :: atom | bitstring | boolean | float | function | integer | list | pid | port | reference | tuple.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check if module exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_module_exists(Module) -> boolean() when
    Module :: atom().
is_module_exists(Module) ->
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
    X :: term(),
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
    Timestamp :: pos_integer().
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
    ModuleName :: atom(),
    ModuleNameList :: [ModuleName],
    Result :: [{module, ModuleName} | {error, term()}].
hot_code_replace(ModuleNameList) ->
    [begin code:purge(ModuleName), code:load_file(ModuleName) end || ModuleName <- ModuleNameList].

%%--------------------------------------------------------------------
%% @doc
%% Finds the element position from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec index_of(Item, List) -> Pos when
    List :: [term()],
    Item :: term(),
    Pos :: -1 | pos_integer().
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
    PidOrName :: pid() | atom(),
    DetectPeriodInMilli :: pos_integer().
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

%%%===================================================================
%%% Internal functions
%%%===================================================================