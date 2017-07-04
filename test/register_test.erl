%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(register_test).
-author("shuieryin").

-import(wechat_mud_SUITE, [mod_input/2, mod_input/3]).

%% API
-export([
]).

-export([
    test/1,
    test_id/1
]).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    RawTestUid = elib:uuid(),
    TestUid = atom_to_binary(RawTestUid, utf8),
    TestId = test_id(TestUid),
    FsmId = register_statem:register_server_name(RawTestUid),

    % First time - START
    mod_input(TestUid, <<"afsdf">>),
    mod_input(TestUid, <<"zh">>),

    normal_reg_flow(TestUid, TestId),
    mod_input(TestUid, <<"event">>, <<"unsubscribe">>),
    % First time - END

    % Second time - START
    mod_input(TestUid, <<"afsdf">>),
    mod_input(TestUid, <<"zh">>),

    normal_reg_flow(TestUid, TestId),
    mod_input(TestUid, <<"n">>),
    % Second time - END

    MonitorRef = monitor(process, FsmId),

    % Third time - START
    normal_reg_flow(TestUid, TestId),
    register_statem:current_player_profile(RawTestUid),
    mod_input(TestUid, <<>>),
    mod_input(TestUid, <<"y">>),
    % Third time - END

    receive
        {'DOWN', MonitorRef, process, _, _} ->
            true
    end.

test_id(TestUid) ->
    [TestId | _] = re:split(TestUid, "-", [{return, binary}]),
    TestId.

%%%===================================================================
%%% Internal functions
%%%===================================================================
normal_reg_flow(TestUid, TestId) ->
    mod_input(TestUid, <<"@!!$@#$fa">>),
    mod_input(TestUid, <<"ajk2l23j4k2l3jsasaedf">>),

    LoggedInUidsSet = login_server:logged_in_player_uids(),
    case ?ONE_OF(gb_sets:to_list(LoggedInUidsSet)) of
        undefined ->
            ok;
        ExistingUid ->
            ExistingPlayerId = player_statem:player_id(ExistingUid),
            mod_input(TestUid, ExistingPlayerId)
    end,

    mod_input(TestUid, TestId),

    mod_input(TestUid, <<"afsdf">>),
    mod_input(TestUid, <<>>),
    mod_input(TestUid, ?ONE_OF([<<"m">>, <<"f">>])),

    mod_input(TestUid, <<"15">>),
    mod_input(TestUid, <<"asdjk">>),
    mod_input(TestUid, integer_to_binary(?ONE_OF(lists:seq(1, 12)))),

    mod_input(TestUid, <<"nfajsld">>).