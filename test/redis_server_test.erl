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

-define(SERVER, redis_client_server).

-include_lib("wechat_mud_test.hrl").

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    ?GEN_TEST(100).

command(_ModelState) ->
    RandomInteger = binary_to_atom(integer_to_binary(random:uniform(100000)), utf8),
    oneof([
        {call, ?SERVER, set, [RandomInteger, RandomInteger, false]},
        {call, ?SERVER, async_set, [RandomInteger, RandomInteger, false]}
    ]).

assert_redis_set(Key, OriValue) ->
    Value = ?SERVER:get(Key),
    Value = OriValue,
    ?SERVER:del([Key], false),
    DeletedValue = ?SERVER:get(Key),
    undefined == DeletedValue.

initial_state() ->
    {}.

next_state(ModelState, _Var, {call, ?SERVER, _Action, _Args}) ->
    ModelState.

precondition(_ModelState, {call, ?SERVER, _Action, _Args}) ->
    true.

postcondition(_ModelState, {call, ?SERVER, set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue);
postcondition(_ModelState, {call, ?SERVER, async_set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================