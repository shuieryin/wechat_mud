#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname incre_ver verbose

%% noinspection ErlangUnusedFunction
main([CurVersion]) ->
    try
	io:format("~p~n", [increase_vsn(CurVersion, 3, 1)])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: version x.x.x\n"),
    halt(1).

increase_vsn(SourceVersion, VersionDepth, Increment) ->
    string:join(increase_vsn(string:tokens(SourceVersion, "."), VersionDepth, Increment, 1, []), ".").
increase_vsn([], _, _, _, AccVersion) ->
    lists:reverse(AccVersion);
increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    UpdatedVersionNum =
        case CurDepth =:= VersionDepth of
            true ->
                integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment);
            _ ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]).

