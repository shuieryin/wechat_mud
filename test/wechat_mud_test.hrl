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

-define(ONE_OF(List), cm:random_from_list(List)).

%% =======================Gen server macros - Start=======================
-define(SERVER_TEST_FULL(Setup, Cleanup, Opts), ?assert(proper:quickcheck(?FORALL(_Cmds, commands(?MODULE),
    ?TRAPEXIT(
        begin
            Setup,
            {History, State, Result} = run_commands(?MODULE, _Cmds),
            Cleanup,
            ?WHENFAIL(ct:pal("Module:~w~nHistory: ~w~nState: ~w~nResult: ~w~n",
                [?MODULE, History, State, Result]),
                aggregate(command_names(_Cmds), Result =:= ok)
            )
        end
    )
), Opts))).

-define(SERVER_TEST_SHORT(Setup, Cleanup), ?SERVER_TEST_FULL(Setup, Cleanup, [])).

-define(SERVER_TEST_OPTS(Opts), ?SERVER_TEST_FULL(ok, ok, Opts)).

-define(ST(), ?SERVER_TEST_OPTS([])).
%% =======================Gen server macros - End=========================

%% =======================Gen fsm macros - Start=======================
-define(FSM_TEST_FULL(Setup, Cleanup, Opts), ?assert(proper:quickcheck(?FORALL(_Cmds, proper_fsm:commands(?MODULE),
    begin
        Setup,
        {History, State, Result} = proper_fsm:run_commands(?MODULE, _Cmds),
        Cleanup,
        ?WHENFAIL(ct:pal("Module:~w~nHistory: ~w~nState: ~w~nResult: ~w~n",
            [?MODULE, History, State, Result]),
            aggregate(zip(proper_fsm:state_names(History), command_names(_Cmds)), Result =:= ok))
    end
), Opts))).

-define(FSM_TEST_SHORT(Setup, Cleanup), ?FSM_TEST_FULL(Setup, Cleanup, [])).

-define(FSM_TEST_OPTS(Opts), ?FSM_TEST_FULL(ok, ok, Opts)).

-define(FT(), ?FSM_TEST_OPTS([])).
%% =======================Gen fsm macros - End=========================