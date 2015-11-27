%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 7:42 PM
%%%-------------------------------------------------------------------
-module(wechat_mud_SUITE).
-author("shuieryin").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0,
    suite/0
]).

-export([
    prop_ordered/1,
    prop_same_length/1,
    prop_same_length_conditional_check/1,
    prop_same_length_no_dupls/1,
    prop_equiv_usort/1
]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SORT_TEST(Input, Test, Opts), ?assert(proper:quickcheck(?FORALL(_I, Input, Test), Opts))).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
suite() ->
    [].

all() -> [{group, test_group1}].

groups() ->
    [{
        test_group1,
        [],
        [
            prop_ordered,
            prop_same_length,
            prop_same_length_conditional_check,
            prop_same_length_no_dupls,
            prop_equiv_usort
        ]
    }].

% {on_output, fun ct:pal/2}
prop_ordered(_Config) ->
    ?SORT_TEST(
        list(integer()),
        ordered(cm:sort(_I)),
        []
    ).

prop_same_length(_Config) ->
    ?SORT_TEST(
        list_no_dupls(integer()), % list(integer()),
        length(_I) =:= length(cm:sort(_I)),
        50
    ).

prop_same_length_conditional_check(_Config) ->
    ?SORT_TEST(
        list(integer()),
        ?IMPLIES(no_duplicates(_I), length(_I) =:= length(cm:sort(_I))),
        []
    ).

prop_same_length_no_dupls(_Config) ->
    ?SORT_TEST(
        list_no_dupls(integer()),
        length(_I) =:= length(cm:sort(_I)),
        []
    ).

prop_equiv_usort(_Config) ->
    ?SORT_TEST(
        list(integer()),
        cm:sort(_I) =:= lists:usort(_I),
        []
    ).

%%%===================================================================
%%% Init states
%%%===================================================================
init_per_suite(Config) ->
%%    Result = os:cmd("cd ../..; make run"),
%%    ct:pal("==========================Started:~n"),
    Config.

end_per_suite(_Config) ->
%%    ct:pal("==========================Stopped:~n"),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
no_duplicates([]) -> true;
no_duplicates([A | T]) ->
    not lists:member(A, T) andalso no_duplicates(T).

ordered([]) -> true;
ordered([_]) -> true;
ordered([A, B | T]) -> A =< B andalso ordered([B | T]).

list_no_dupls(T) ->
    ?LET(_I, list(T), remove_duplicates(_I)).

remove_duplicates([]) -> [];
remove_duplicates([A | T]) ->
    case lists:member(A, T) of
        true -> remove_duplicates(T);
        false -> [A | remove_duplicates(T)]
    end.