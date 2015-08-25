-module(wechat_controller).
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

-define(ISDEBUG, true).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Validate signature and handel message
%%
%% @end
%%--------------------------------------------------------------------
start(Req) ->
    case cowboy_req:qs(Req) of
        <<>> ->
            case ?ISDEBUG of
                true ->
                    process_request(Req);
                _ ->
                    error_logger:error_msg("Header params empty~n", []),
                    ""
            end;
        HeaderParams ->
            ParamsMap = gen_qs_params_map(size(HeaderParams) - 1, HeaderParams, #{}),
            error_logger:info_msg("ParamsMap:~p~n", [ParamsMap]),
            ValidationParamsMap = maps:with([signature, timestamp, nonce], ParamsMap),
            case validate_signature(ValidationParamsMap) of
                true ->
                    case maps:is_key(echostr, ParamsMap) of
                        false ->
                            process_request(Req);
                        _ ->
                            error_logger:info_msg("Connectivity success~n", []),
                            maps:get(echostr, ParamsMap)
                    end;
                _ ->
                    ""
            end
    end.

gen_qs_params_map(-1, _, ParamsMap) ->
    ParamsMap;
gen_qs_params_map(Pos, Bin, ParamsMap) ->
    {ValueBin, CurPosByValue} = qs_value(binary:at(Bin, Pos), [], Pos - 1, Bin),
    {KeyBin, CurPosByKey} = qs_key(binary:at(Bin, CurPosByValue), [], CurPosByValue - 1, Bin),
    gen_qs_params_map(CurPosByKey, Bin, maps:put(binary_to_atom(KeyBin, unicode), ValueBin, ParamsMap)).
qs_key($&, KeyBin, Pos, _) ->
    {list_to_binary(KeyBin), Pos};
qs_key(CurByte, KeyBinList, -1, _) ->
    {list_to_binary([CurByte | KeyBinList]), -1};
qs_key(CurByte, KeyBinList, Pos, Bin) ->
    qs_key(binary:at(Bin, Pos), [CurByte | KeyBinList], Pos - 1, Bin).
qs_value($=, ValueBinList, Pos, _) ->
    {list_to_binary(ValueBinList), Pos};
qs_value(CurByte, ValueBinList, Pos, Bin) ->
    qs_value(binary:at(Bin, Pos), [CurByte | ValueBinList], Pos - 1, Bin).


%% get GET params
%%     Params = cowboy_req:qs(Req),
%%     io:format("Params:~p~n", [Params]),
%%     binary_to_list(Params).

%% get POST params
%%     {ok, Result, _} = cowboy_req:body_qs(Req),
%%     io:format("Result:~p~n", [Result]),
%%     "test".

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_signature(OriginalParamMap) ->
    ParamList = maps:to_list(maps:without([signature], OriginalParamMap)),
    GeneratedSignature = generate_signature(ParamList),
    Signature = binary_to_list(maps:get(signature, OriginalParamMap)),
    case GeneratedSignature == Signature of
        true ->
            true;
        _ ->
            error_logger:error_msg("Validation signature failed:~nParamMap:~p~nGSignature:~p~nOSignature:~p~n", [OriginalParamMap, GeneratedSignature, Signature]),
            false
    end.

generate_signature(OriginParamList) ->
    ConcatedParamContent = [<<"collinguo">> | concat_param_content(OriginParamList, [])],
    SortedParamContent = lists:sort(ConcatedParamContent),
    string:to_lower(sha1:hexstring(SortedParamContent)).

process_request(Req) ->
    case parse_xml_request(Req) of
        parse_failed ->
            "";
        ReqParamsMap ->
            error_logger:info_msg("ReqParamsMap:~p~n", [ReqParamsMap]),

            PlatformId = maps:get('ToUserName', ReqParamsMap),
            UserId = maps:get('FromUserName', ReqParamsMap),
            _MsgId = maps:get('MsgId', ReqParamsMap),

            Response = reply_text(UserId, PlatformId, maps:get('Content', ReqParamsMap) ++ pid_to_list(self())),
            error_logger:info_msg("Response:~p~n", [Response]),
            Response
    end.

reply_text(UserId, PlatformId, Content) ->
    "<xml><ToUserName><![CDATA[" ++ UserId ++ "]]></ToUserName><FromUserName><![CDATA[" ++ PlatformId ++ "]]></FromUserName><CreateTime>" ++ integer_to_list(timestamp()) ++ "</CreateTime><MsgType><![CDATA[text]]></MsgType><Content><![CDATA[" ++ Content ++ "]]></Content></xml>".

%%--------------------------------------------------------------------
%% @private
%% @doc
%% [{"ToUserName", [], [PlatFormId]},
%% {"FromUserName", [], [UserId]},
%% {"CreateTime", [], [CreateTime]},
%% {"MsgType", [], [MsgType]},
%% {"Content", [], [Content]},
%% {"MsgId", [], [MsgId]}]
%%
%% @end
%%--------------------------------------------------------------------
parse_xml_request(Req) ->
    {ok, Message, _} = cowboy_req:body(Req),
    error_logger:info_msg("Message:~p~n", [Message]),
    {ok, {"xml", [], Params}, _} = erlsom:simple_form(binary_to_list(Message)),
    unmarshall_params(Params, #{}).

unmarshall_params([], ParamsMap) ->
    ParamsMap;
unmarshall_params([{Key, [], [Value]} | Tail], ParamsMap) ->
    unmarshall_params(Tail, maps:put(list_to_atom(Key), Value, ParamsMap)).

concat_param_content([], ConcatedParamContent) ->
    lists:reverse(ConcatedParamContent);
concat_param_content([{_, ParamContent} | Tail], ConcatedParamContent) ->
    concat_param_content(Tail, [ParamContent | ConcatedParamContent]).

timestamp() ->
    {Hour, Minute, _} = os:timestamp(),
    Hour * 1000000 + Minute.