%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2016 4:18 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(skill, {
    skill_id :: player_fsm:skill_id(),
    skill_seq :: integer(),
    damage_formula :: erl_eval:expression_list(),
    buff :: [term()] % generic term
}).