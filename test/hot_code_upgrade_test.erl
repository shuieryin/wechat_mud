%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2015 8:09 PM
%%%-------------------------------------------------------------------
-module(hot_code_upgrade_test).
-author("shuieryin").

-import(wechat_mud_SUITE, [mod_input/2]).

%% API
-export([
    test/1
]).

-include_lib("wechat_mud_test.hrl").
-include_lib("wechat_mud/src/data_type/npc_profile.hrl").
-include_lib("wechat_mud/src/data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

test(_Config) ->
    PrivDir = code:priv_dir(wechat_mud),
    NlsFilesPath = filename:join([PrivDir, "nls_server"]),
    {ok, NlsFileNames} = file:list_dir(NlsFilesPath),
    TargetNlsFilePath = filename:join([NlsFilesPath, ?ONE_OF(NlsFileNames)]),
    TotalLineNum = list_to_integer(os:cmd("wc -l " ++ TargetNlsFilePath ++ " | grep -oP \"\\s*\\K(\\d+)\" | tr -d '\n'")),
    RandomLineNum = rand:uniform(TotalLineNum - 1),

    {ok, TargetFile} = file:open(TargetNlsFilePath, [read]),
    TargetLineNum = if
                        RandomLineNum == 1 ->
                            RandomLineNum + 1;
                        true ->
                            RandomLineNum
                    end,

    {OriginLine, PrefixContent, SuffixContent} = get_line(TargetFile, TargetLineNum),
    {match, [_OriginLineBin, LinePrefix, OriginLineSuffix]} = re:run(OriginLine, <<"(.*),(.*)">>, [{capture, all, binary}]),

    EndingChar = case re:run(OriginLineSuffix, <<"\"$">>) of
                     nomatch ->
                         <<>>;
                     {match, _Match} ->
                         <<"\"">>
                 end,

    file:write_file(TargetNlsFilePath, <<PrefixContent/binary, LinePrefix/binary, ",233333", EndingChar/binary, "\n", SuffixContent/binary>>),

    error_logger:info_msg("start hcu"),

    os:cmd("cd " ++ filename:join([PrivDir, ".."]) ++ "; make hcu"), % TODO try to implement black box hot code upgrade

    file:write_file(TargetNlsFilePath, <<PrefixContent/binary, LinePrefix/binary, ",", OriginLineSuffix/binary, "\n", SuffixContent/binary>>),
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_line(File, AccLineNum) ->
    get_line(File, AccLineNum, <<>>, <<>>, <<>>).
get_line(File, AccLineNum, TargetLine, AccPrefixContent, AccSuffixContent) ->
    case io:get_line(File, "") of
        eof ->
            file:close(File),
            {TargetLine, AccPrefixContent, AccSuffixContent};
        Line ->
            LineBin = list_to_binary(Line),

            if
                AccLineNum == 0 ->
                    case re:run(LineBin, <<"\"$">>) of
                        nomatch ->
                            get_line(File, AccLineNum - 1, LineBin, AccPrefixContent, AccSuffixContent);
                        {match, _Match} ->
                            get_line(File, AccLineNum, TargetLine, <<AccPrefixContent/binary, LineBin/binary>>, AccSuffixContent)
                    end;
                AccLineNum < 0 ->
                    get_line(File, AccLineNum - 1, TargetLine, AccPrefixContent, <<AccSuffixContent/binary, LineBin/binary>>);
                true ->
                    get_line(File, AccLineNum - 1, TargetLine, <<AccPrefixContent/binary, LineBin/binary>>, AccSuffixContent)
            end
    end.