%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(logout_test).
-author("shuieryin").

-import(wechat_mud_SUITE, [mod_input/2]).

%% API
-export([
    test/1
]).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_profile.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    LoggedInUidsSet = login_server:logged_in_player_uids(),
    CurrentPlayerUid = ?ONE_OF(gb_sets:to_list(LoggedInUidsSet)),
    mod_input(CurrentPlayerUid, <<"logout">>).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================