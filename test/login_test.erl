%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(login_test).
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
    RegisteredUid = ?ONE_OF(login_server:registered_player_uids()),
    case whereis(RegisteredUid) of
        undefined ->
            mod_input(RegisteredUid, <<"login">>);
        _LoggedIn ->
            ok
    end,
    mod_input(RegisteredUid, <<"logout">>).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================