-module(wechat_docking).
%% API
-export([start/1]).

%%%-------------------------------------------------------------------
%%% @author Li
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 10:06 PM
%%%-------------------------------------------------------------------
-author("Li").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
start(Req) ->
%%     io:format("Req:~p~n", [Req]),

%%     Params = cowboy_req:qs(Req),
%%     io:format("Params:~p~n", [Params]),
%%     binary_to_list(Params).

    OriginalParamMap = cowboy_req:match_qs([signature, timestamp, nonce, echostr], Req),
    ParamMap = maps:put(token, <<"collinguo">>, OriginalParamMap),
    ParamList = maps:to_list(maps:without([signature, echostr], ParamMap)),
    ConcatedParamContent = concat_param_content(ParamList, []),
    SortedParamContent = lists:sort(ConcatedParamContent),
    GeneratedSignature = string:to_lower(sha1:hexstring(SortedParamContent)),
    Signature = binary_to_list(maps:get(signature, ParamMap)),
    Result = case GeneratedSignature == Signature of
                 true ->
                     maps:get(echostr, ParamMap);
                 _ ->
                     error_logger:error_msg("Wechat docking failed"),
                     error_logger:error_msg("ParamMap:~p~n", [ParamMap]),
                     error_logger:error_msg("~nGSignature:~p~nOSignature:~p~n", [GeneratedSignature, Signature]),
                     "false"
             end,
    Result.

%%     {ok, Result, _} = cowboy_req:body_qs(Req),
%%     io:format("Result:~p~n", [Result]),
%%     "test".

%%     {ok, [{Bin1, Bin2}], _} = cowboy_req:body_qs(Req),
%%     io:format("Bin1:~p~nBin2:~p~n", [Bin1, Bin2]),
%%     binary_to_list(Bin1) ++ binary_to_list(Bin2).

%%     {ok, Headers, _} = cowboy_req:part(Req),
%%     {data, FormData} = cow_multipart:form_data(Headers),
%%     io:format("Headers:~p~nFormData:~p~n", [Headers, FormData]),
%%     binary_to_list(FormData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
concat_param_content([], ConcatedParamContent) ->
    lists:reverse(ConcatedParamContent);
concat_param_content([{_, ParamContent} | Tail], ConcatedParamContent) ->
    concat_param_content(Tail, [ParamContent | ConcatedParamContent]).