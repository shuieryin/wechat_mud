%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(nls_server_test).
-author("shuieryin").

%% API
-export([
    command/1,
    initial_state/0,
    precondition/2,
    postcondition/3,
    next_state/3
]).

-export([
    test/1
]).

-define(SERVER, nls_server).
-define(NLS_LIST_SIZE, 20).

-include_lib("wechat_mud_test.hrl").

-record(state, {
    valid_langs,
    test_langs,
    nls_keys
}).

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    ?ST().

initial_state() ->
    ValidLangs = elib:type_values(?SERVER, support_lang),
    NlsKeys = ?SERVER:lang_map(?ONE_OF(ValidLangs)),

    #state{
        valid_langs = ValidLangs,
        test_langs = ValidLangs ++ [kr, jp],
        nls_keys = NlsKeys
    }.

command(#state{valid_langs = ValidLangs, test_langs = TestLangs, nls_keys = NlsKeys}) ->
    OneofValidLang = ?ONE_OF(ValidLangs),
    oneof([
        {call, ?SERVER, is_valid_lang, [?ONE_OF(TestLangs)]},
        {call, ?SERVER, lang_map, [OneofValidLang]},
        {call, ?SERVER, get_nls_content, [gen_nls_object_list(NlsKeys), OneofValidLang]}
    ]).

next_state(ModelState, _Var, {call, ?SERVER, _Action, _Args}) ->
    ModelState.

precondition(_ModelState, {call, ?SERVER, _Action, _Args}) ->
    true.

postcondition(#state{valid_langs = ValidLangs}, {call, ?SERVER, is_valid_lang, [TargetLang]}, Result) ->
    lists:member(TargetLang, ValidLangs) == Result;
postcondition(_ModelState, {call, ?SERVER, lang_map, _Args}, Result) ->
    map == elib:type_of(Result);
postcondition(_ModelState, {call, ?SERVER, get_nls_content, _Args}, Result) ->
    Type = elib:type_of(list_to_binary(Result)),
    binary == Type.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gen_nls_object_list(LangMap) ->
    RandomSize = random:uniform(?NLS_LIST_SIZE),
    gen_nls_object_list(RandomSize, maps:keys(LangMap), []).

gen_nls_object_list(0, _, AccList) ->
    AccList;
gen_nls_object_list(Size, NlsKeys, AccList) ->
    gen_nls_object_list(Size - 1, NlsKeys, [{nls, ?ONE_OF(NlsKeys)} | AccList]).