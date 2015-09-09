-module(rereg).
%% API
-export([exec/2,
    info/1]).

%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2015 11:28 PM
%%%-------------------------------------------------------------------
-author("Shuieryin").

-include_lib("wechat_mud/src/nls/rereg_nls.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% re-register user
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, UidProfile) -> no_return() when
    UidProfile :: command_dispatcher:uid_profile(),
    DispatcherPid :: pid().
exec(DispatcherPid, UidProfile) ->
    #{uid := Uid} = UidProfile,
    login_server:register_uid(DispatcherPid, Uid).

%%--------------------------------------------------------------------
%% @doc
%% return command info
%%
%% @end
%%--------------------------------------------------------------------
-spec info(Lang) -> binary() when
    Lang :: lang().
info(Lang) ->
    ?NLS(info, Lang).

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