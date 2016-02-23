%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2016 7:05 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(skill_formula, {
    formula :: erl_eval:expression_list(),
    from_var_names :: erl_eval:bindings(),
    to_var_names :: erl_eval:bindings()
}).

-record(skill, {
    skill_id :: player_fsm:skill_id(),
    skill_seq :: integer(),
    skill_formula :: #skill_formula{},
    buff :: [term()] % generic term
}).

-record(perform_args, {
    skill :: #skill{},
    value_bindings :: erl_eval:bindings(),
    perform_results :: [perform:perform_result()]
}).