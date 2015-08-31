-module(login).
%% API
-export([exec/2]).

%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2015 11:01 AM
%%%-------------------------------------------------------------------
-author("Shuieryin").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% log user in
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispacherPid, State) -> string() when
    State :: map(),
    DispacherPid :: pid().
exec(DispacherPid, State) ->
    Uid = maps:get(uid, State),
%%     case login_server:is_uid_registered(Uid) of
%%         false ->
%%             register
%%     end,
%%     atom_to_list(Uid).
    command_dispatcher:return_text(DispacherPid, [<<"success:">>, atom_to_binary(Uid, utf8)]).
%%     register_fsm:start_link(Uid).

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
