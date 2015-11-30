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

initial_state() ->
    gb_sets:new().

command(ModelState) ->
    RandomInteger = binary_to_atom(integer_to_binary(random:uniform(100000)), utf8),
    CurIntegerListSize = gb_sets:size(ModelState),
    RandomDeletes = case CurIntegerListSize of
                        0 ->
                            [];
                        _ ->
                            CurIntegerList = gb_sets:to_list(ModelState),
                            lists:nthtail(random:uniform(CurIntegerListSize), CurIntegerList)
                    end,
    oneof([
        {call, ?SERVER, set, [RandomInteger, RandomInteger, false]},
        {call, ?SERVER, async_set, [RandomInteger, RandomInteger, false]},
        {call, ?SERVER, del, [RandomDeletes, false]},
        {call, ?SERVER, async_del, [RandomDeletes, false]}
    ]).

assert_redis_set(Key, OriValue) ->
    Value = ?SERVER:get(Key),
    Value == OriValue.

assert_redis_del(Keys) ->
    length(Keys) == length([any || Key <- Keys, undefined == ?SERVER:get(Key)]).

next_state(ModelState, _Var, {call, ?SERVER, set, [Key, _, _]}) ->
    gb_sets:add(Key, ModelState);
next_state(ModelState, _Var, {call, ?SERVER, async_set, [Key, _, _]}) ->
    gb_sets:add(Key, ModelState);
next_state(ModelState, _Var, {call, ?SERVER, del, [Keys, _]}) ->
    lists:foldl(
        fun(Elem, AccState) ->
            gb_sets:del_element(Elem, AccState)
        end,
        ModelState,
        Keys);
next_state(ModelState, _Var, {call, ?SERVER, async_del, [Keys, _]}) ->
    lists:foldl(
        fun(Elem, AccState) ->
            gb_sets:del_element(Elem, AccState)
        end,
        ModelState,
        Keys).

precondition(_ModelState, {call, ?SERVER, del, [Keys, _]}) ->
    [] /= Keys;
precondition(_ModelState, {call, ?SERVER, async_del, [Keys, _]}) ->
    [] /= Keys;
precondition(_ModelState, {call, ?SERVER, _Action, _Args}) ->
    true.

postcondition(_ModelState, {call, ?SERVER, set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue);
postcondition(_ModelState, {call, ?SERVER, async_set, [Key, OriValue, _]}, _Result) ->
    assert_redis_set(Key, OriValue);
postcondition(_ModelState, {call, ?SERVER, del, [Keys, _]}, _Result) ->
    assert_redis_del(Keys);
postcondition(_ModelState, {call, ?SERVER, async_del, [Keys, _]}, _Result) ->
    assert_redis_del(Keys).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================