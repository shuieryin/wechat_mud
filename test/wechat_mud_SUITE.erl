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
    redis_server_test/1,
    common_server_test/1,
    nls_server_test/1,
    register_fsm_test/1
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
        [{repeat, 2}], % parallel
        [
            redis_server_test,
            common_server_test,
            nls_server_test,
            register_fsm_test
        ]
    }].

redis_server_test(Cfg) -> redis_server_test:test(Cfg).
common_server_test(Cfg) -> common_server_test:test(Cfg).
nls_server_test(Cfg) -> nls_server_test:test(Cfg).
register_fsm_test(Cfg) -> register_fsm_test:test(Cfg).

%%%===================================================================
%%% Init states
%%%===================================================================
init_per_suite(Config) ->
    error_logger:tty(false),
    spawn(
        fun() ->
            os:cmd("redis-server")
        end),
    redis_client_server:start(),
    common_server:start(),
    nls_server:start(),
    npc_root_sup:start(),
    login_server:start(),
    scene_sup:start(),
    register_fsm_sup:start(),
    player_fsm_sup:start(),
    Config.

end_per_suite(_Config) ->
    register_fsm_sup:stop(),
    scene_sup:stop(),
    login_server:stop(),
    npc_root_sup:stop(),
    nls_server:stop(),
    common_server:stop(),
    redis_client_server:stop(),
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.