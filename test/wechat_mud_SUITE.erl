%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 7:42 PM
%%%-------------------------------------------------------------------
-module(wechat_mud_SUITE).
-author("shuieryin").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0,
    suite/0
]).

-export([
    rst/1
]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
suite() ->
    [].

all() ->
    [
        {group, servers}
    ].

groups() ->
    [{
        servers,
        [parallel, {repeat, 2}],
        [
            rst
        ]
    }].

rst(Cfg) -> redis_server_test:test(Cfg).

%%%===================================================================
%%% Init states
%%%===================================================================
init_per_suite(Config) ->
    spawn(
        fun() ->
            os:cmd("redis-server")
        end),
    Config.

end_per_suite(_Config) ->
    spawn(
        fun() ->
            os:cmd("redis-cli shutdown")
        end),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    redis_client_server:start_link(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    redis_client_server:stop(),
    ok.