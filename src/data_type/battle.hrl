%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2016 2:43 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(battle_status, {
    attack :: integer(),
    l_attack :: integer(),
    defence :: integer(),
    l_defence :: integer(),
    hp :: integer(),
    l_hp :: integer(),
    dexterity :: integer(),
    l_dexterity :: integer()
}).