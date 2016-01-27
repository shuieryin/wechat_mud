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
    'Strength' :: integer(),
    'M_attack' :: integer(),
    'Defense' :: integer(),
    'M_defense' :: integer(),
    'Hp' :: integer(),
    'L_hp' :: integer(),
    'Dexterity' :: integer(),
    'L_dexterity' :: integer()
}).