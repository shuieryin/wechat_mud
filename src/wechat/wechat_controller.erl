-module(wechat_controller).
%% API
-export([start/1]).

%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 10:06 PM
%%%-------------------------------------------------------------------
-author("Shuieryin").

-define(IS_DEBUG, true).
-define(MAX_TEXT_SIZE, 2048).

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
            case ?IS_DEBUG of
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
            #{'MsgType' := MsgType, 'ToUserName' := PlatformId, 'FromUserName' := Uid} = ReqParamsMap,

            Response = case list_to_atom(MsgType) of
                           event ->
                               case list_to_atom(maps:get('Event', ReqParamsMap)) of
                                   subscribe ->
                                       reply_text(Uid, PlatformId, "欢迎加入");
                                   unsubscribe ->
                                       "";
                                   _ ->
                                       ""
                               end;
                           text ->
                               _MsgId = maps:get('MsgId', ReqParamsMap),
                               [ModuleName | CommandArgs] = string:tokens(maps:get('Content', ReqParamsMap), " "),
                               ReplyText = try
                                               false = is_integer(ModuleName),
                                               Module = list_to_atom(ModuleName),
                                               true = common_api:is_module_exists(Module),

                                               StateMap = #{uid => list_to_atom(Uid), args => CommandArgs},
                                               error_logger:info_msg("Executing Module:~p, Args:~p~n", [Module, CommandArgs]),
                                               apply(Module, exec, [StateMap])
                                           catch
                                               error:_ ->
                                                   "不支持该指令: " ++ ModuleName;
                                               Type:Reason ->
                                                   error_logger:error_msg("Command error~n Type:~p~nReason:~p~n", [Type, Reason]),
                                                   "非法指令"
                                           end,
                               reply_text(Uid, PlatformId, ReplyText);
                           _ ->
                               reply_text(Uid, PlatformId, "暂不支持该类型信息")
                       end,

            error_logger:info_msg("Response:~ts~n", [Response]),
            Response
    end.

reply_text(Uid, PlatformId, Content) ->
    list_to_binary([<<"<xml><Content><![CDATA[">>, unicode:characters_to_binary(Content), <<"]]></Content><ToUserName><![CDATA[">>, list_to_binary(Uid), <<"]]></ToUserName><FromUserName><![CDATA[">>, list_to_binary(PlatformId), <<"]]></FromUserName><CreateTime>">>, integer_to_binary(timestamp()), <<"</CreateTime><MsgType><![CDATA[text]]></MsgType></xml>">>]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% [{"ToUserName", [], [PlatFormId]},
%% {"FromUserName", [], [Uid]},
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
    case Message of
        <<>> ->
            parse_failed;
        _ ->
            {ok, {"xml", [], Params}, _} = erlsom:simple_form(Message),
            unmarshall_params(Params, #{})
    end.

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