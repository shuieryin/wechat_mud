%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2016 10:19 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").

-record(affair_context, {
    from_player :: #player_profile{} | undefined,
    affair_name :: binary() | undefined,
    answer :: #ask_n_answer{} | undefined,
    dispatcher_pid :: pid() | undefined,
    response_message :: [nls_server:nls_object()] | undefined
}).