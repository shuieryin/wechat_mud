#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname generate_appup verbose

start() ->
    Output = os:cmd("rebar3 path --app wechat_mud"),
    io:format("~p~n", [Output])
   

main([]) ->
    try
	start()
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: version x.x.x\n"),
    halt(1).

generate_appups(Map, State) when is_map(Map) ->
    maps:map(fun(K,V) ->
        create_appup_term(generate_appup(K,V,State))
    end, Map).
