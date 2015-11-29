%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(redis_server_test).
-author("shuieryin").

%% API
-export([
    command/1,
    initial_state/0,
    precondition/2,
    postcondition/3,
    next_state/3
]).

-export([
    test/1
]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    ?assert(proper:quickcheck(?FORALL(_Cmds, commands(?MODULE),
        ?TRAPEXIT(
            begin
                {History, State, Result} = run_commands(?MODULE, _Cmds),
                ?WHENFAIL(ct:pal("History: ~w~nState: ~w~nResult: ~w~n",
                    [History, State, Result]),
                    aggregate(command_names(_Cmds), Result =:= ok)
                )
            end
        )
    ), 100)).

command(_ModelState) ->
    RandomInteger = binary_to_atom(integer_to_binary(random:uniform(100000)), utf8),
    oneof([
        {call, redis_client_server, set, [RandomInteger, RandomInteger, false]},
        {call, redis_client_server, async_set, [RandomInteger, RandomInteger, false]}
    ]).

assert_redis_set(Key, OriValue) ->
    Value = redis_client_server:get(Key),
    ?assert(Value =:= OriValue),
    redis_client_server:del([Key], false),
    DeletedValue = redis_client_server:get(Key),
    ?assert(undefined == DeletedValue),
    true.

initial_state() ->
    {}.

next_state(ModelState, _Var, {call, redis_client_server, _Action, _Args}) ->
    ModelState.

precondition(_ModelState, {call, redis_client_server, _Action, _Args}) ->
    true.

postcondition(_ModelState, {call, redis_client_server, set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue);
postcondition(_ModelState, {call, redis_client_server, async_set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================