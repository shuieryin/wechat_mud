-module(login).
%% API
-export([exec/1]).

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
-spec exec(State) -> string() when
    State :: map().
exec(State) ->
    Uid = maps:get(uid, State),
%%     case login_server:(Uid) of
%%         false ->
%%
%%     end,
%%     io:format("is_uid_registered:~p~n", [IsUidExist]),
    atom_to_list(Uid).

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
