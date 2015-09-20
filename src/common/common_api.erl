-module(common_api).
%% API
-export([is_module_exists/1,
    type_of/1,
    timestamp/0,
    reload_modules/1,
    index_of/2,
    list_has_element/2]).

%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-author("Shuieryin").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Test whether a module exists
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
%% type detection
%%
%% @end
%%--------------------------------------------------------------------
-spec type_of(X) -> Result when
    X :: term(),
    Result :: atom | bitstring | boolean | float | function | integer | list | pid | port | reference | tuple | unknown.
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
%% timestamp long
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> integer().
timestamp() ->
    {Hour, Minute, _} = os:timestamp(),
    Hour * 1000000 + Minute.

%%--------------------------------------------------------------------
%% @doc
%% timestamp long
%%
%% @end
%%--------------------------------------------------------------------
-spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}].
reload_modules(Modules) ->
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

%%--------------------------------------------------------------------
%% @doc
%% find elememnt pos from list
%%
%% @end
%%--------------------------------------------------------------------
-spec index_of(List, Item) -> integer() when
    List :: [term()],
    Item :: term().
index_of(List, Item) ->
    index_of(List, Item, 0).
index_of([], _, _) ->
    -1;
index_of([Elem | _], Elem, Pos) ->
    Pos;
index_of([_ | Tail], Item, Pos) ->
    index_of(Tail, Item, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% find elememnt pos from list
%%
%% @end
%%--------------------------------------------------------------------
-spec list_has_element(List, Item) -> boolean() when
    List :: [term()],
    Item :: term().
list_has_element(List, Item) ->
    case index_of(List, Item) of
        -1 ->
            false;
        _ ->
            true
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------