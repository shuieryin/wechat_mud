%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2015 11:10 AM
%%%-------------------------------------------------------------------
-author("shuieryin").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GEN_TEST_FULL(Setup, Cleanup, Opts), ?assert(proper:quickcheck(?FORALL(_Cmds, commands(?MODULE),
    ?TRAPEXIT(
        begin
            Setup,
            {History, State, Result} = run_commands(?MODULE, _Cmds),
            Cleanup,
            ?WHENFAIL(ct:pal("History: ~w~nState: ~w~nResult: ~w~n",
                [History, State, Result]),
                aggregate(command_names(_Cmds), Result =:= ok)
            )
        end
    )
), Opts))).

-define(GEN_TEST(Opts), ?GEN_TEST_FULL(ok, ok, Opts)).

-define(GT(), ?GEN_TEST([])).

-define(ONE_OF(List), cm:random_from_list(List)).