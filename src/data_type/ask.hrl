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
    from_player :: #player_profile{},
    to_target :: #player_profile{} | #npc_state{},
    affair_name :: binary(),
    answer :: #ask_n_answer{},
    dispatcher_pid :: pid()
}).