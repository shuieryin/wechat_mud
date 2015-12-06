%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(login_server_test).
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

-define(SERVER, login_server).

-include_lib("wechat_mud_test.hrl").

-record(state, {
    registered_uids_set
}).

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    ?SERVER_TEST_OPTS(20).

initial_state() ->
    RegisteredUidsSet = login_server:get_registered_player_uids(),

    #state{
        registered_uids_set = RegisteredUidsSet
    }.

command(#state{registered_uids_set = RegisteredUidsSet}) ->
    UidsList = gb_sets:to_list(RegisteredUidsSet),
    Self = self(),
    TestUid = ?ONE_OF(UidsList),
    oneof([
        {call, ?SERVER, login, [Self, TestUid]},
        {call, ?SERVER, logout, [Self, TestUid]}
    ]).

next_state(#state{registered_uids_set = RegisterdUidsSet} = ModelState, _Var, {call, ?SERVER, logout, [_, LogoutPlayerUid]}) ->
    ModelState#state{registered_uids_set = gb_sets:del_element(LogoutPlayerUid, RegisterdUidsSet)};
next_state(ModelState, _Var, {call, ?SERVER, _Action, _Args}) ->
    ModelState.

precondition(_ModelState, {call, ?SERVER, logout, [_, LogoutPlayerUid]}) ->
    login_server:is_uid_logged_in(LogoutPlayerUid);
precondition(_ModelState, {call, ?SERVER, _Action, _Args}) ->
    true.

postcondition(_ModelState, {call, ?SERVER, login, _Args}, _Result) ->
    _Result == ok;
postcondition(_ModelState, {call, ?SERVER, logout, _Args}, _Result) ->
    _Result == ok.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================