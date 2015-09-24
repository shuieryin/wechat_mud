%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 9:20 PM
%%%-------------------------------------------------------------------
-module(web_req_dispatcher).
-author("Shuieryin").

%% API
-export([dispatch/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
dispatch(F) ->
    F1 = dispatch1(F),
    io:format("web_req_dispatcher::dispatch ~s => ~s~n", [F, F1]),
    F1.

dispatch1("/ezwebframe/" ++ F) ->
    Dir = dir(2, code:which(ezwebframe)) ++ "/priv/",
    Dir ++ F;
dispatch1("/" ++ F) ->
    Dir = dir(2, code:which(?MODULE)) ++ "/",
    Dir ++ F.

dir(0, F) -> F;
dir(K, F) -> dir(K - 1, filename:dirname(F)).

%%%===================================================================
%%% Internal functions
%%%===================================================================