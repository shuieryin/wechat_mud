-module(command_dispatcher).
%% API
-export([start/1,
    return_text/2]).

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
-define(EMPTY_RESPONSE, <<>>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Validate signature and handel message
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Req) -> string() when
    Req :: cowboy_req:req().
start(Req) ->
    case cowboy_req:qs(Req) of
        <<>> ->
            case ?IS_DEBUG of
                true ->
                    process_request(Req);
                _ ->
                    error_logger:error_msg("Header params empty~n", []),
                    ?EMPTY_RESPONSE
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
                    ?EMPTY_RESPONSE
            end
    end.

-spec gen_qs_params_map(Pos, Bin, ParamsMap) -> map() when
    Pos :: integer(),
    Bin :: binary(),
    ParamsMap :: map().
gen_qs_params_map(-1, _, ParamsMap) ->
    ParamsMap;
gen_qs_params_map(Pos, Bin, ParamsMap) ->
    {ValueBin, CurPosByValue} = qs_value(binary:at(Bin, Pos), [], Pos - 1, Bin),
    {KeyBin, CurPosByKey} = qs_key(binary:at(Bin, CurPosByValue), [], CurPosByValue - 1, Bin),
    gen_qs_params_map(CurPosByKey, Bin, maps:put(binary_to_atom(KeyBin, unicode), ValueBin, ParamsMap)).

-spec qs_key(CurByte, KeyBinList, Pos, SrcBin) -> binary() when
    CurByte :: binary(),
    KeyBinList :: list(),
    Pos :: integer(),
    SrcBin :: binary().
qs_key($&, KeyBinList, Pos, _) ->
    {list_to_binary(KeyBinList), Pos};
qs_key(CurByte, KeyBinList, -1, _) ->
    {list_to_binary([CurByte | KeyBinList]), -1};
qs_key(CurByte, KeyBinList, Pos, SrcBin) ->
    qs_key(binary:at(SrcBin, Pos), [CurByte | KeyBinList], Pos - 1, SrcBin).

-spec qs_value(CurByte, ValueBinList, Pos, SrcBin) -> binary() when
    CurByte :: binary(),
    ValueBinList :: list(),
    Pos :: integer(),
    SrcBin :: binary().
qs_value($=, ValueBinList, Pos, _) ->
    {list_to_binary(ValueBinList), Pos};
qs_value(CurByte, ValueBinList, Pos, SrcBin) ->
    qs_value(binary:at(SrcBin, Pos), [CurByte | ValueBinList], Pos - 1, SrcBin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec validate_signature(OriginalParamMap) -> boolean() when
    OriginalParamMap :: map().
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

-spec generate_signature(OriginParamList) -> string() when
    OriginParamList :: list().
generate_signature(OriginParamList) ->
    ConcatedParamContent = [<<"collinguo">> | concat_param_content(OriginParamList, [])],
    SortedParamContent = lists:sort(ConcatedParamContent),
    string:to_lower(sha1:hexstring(SortedParamContent)).

-spec process_request(Req) -> string() when
    Req :: cowboy_req:req().
process_request(Req) ->
    case parse_xml_request(Req) of
        parse_failed ->
            ?EMPTY_RESPONSE;
        ReqParamsMap ->
            error_logger:info_msg("ReqParamsMap:~p~n", [ReqParamsMap]),
            #{'MsgType' := MsgType, 'ToUserName' := PlatformId, 'FromUserName' := UidBin} = ReqParamsMap,

            Uid = binary_to_atom(UidBin, utf8),
            ReplyText = case binary_to_atom(MsgType, utf8) of
                            event ->
                                case binary_to_atom(maps:get('Event', ReqParamsMap), utf8) of
                                    subscribe ->
                                        pending_text(login_server, register_uid, [Uid]);
                                    unsubscribe ->
                                        ?EMPTY_RESPONSE;
                                    _ ->
                                        ?EMPTY_RESPONSE
                                end;
                            text ->
                                % _MsgId = maps:get('MsgId', ReqParamsMap),
                                % todo: check if there's regsiter fsm, process it if exists.
                                RawInput = maps:get('Content', ReqParamsMap),
                                [ModuleName | CommandArgs] = binary:split(RawInput, <<" ">>),
                                case login_server:is_in_registration(Uid) of
                                    true ->
                                        pending_text(register_fsm, input, [Uid, RawInput]);
                                    _ ->
                                        try
                                            false = is_integer(ModuleName),
                                            Module = binary_to_atom(ModuleName, utf8),
                                            true = common_api:is_module_exists(Module),

                                            StateMap = #{uid => Uid, args => CommandArgs},
                                            error_logger:info_msg("Executing Module:~p, Args:~p~n", [Module, CommandArgs]),

                                            pending_text(Module, exec, [StateMap])
                                        catch
                                            error:_ ->
                                                [<<"不支持该指令: "/utf8>>, ModuleName];
                                            Type:Reason ->
                                                error_logger:error_msg("Command error~n Type:~p~nReason:~p~n", [Type, Reason]),
                                                <<"非法指令"/utf8>>
                                        end
                                end;
                            _ ->
                                <<"暂不支持该类型信息"/utf8>>
                        end,

            error_logger:info_msg("ReplyText:~p~n", [ReplyText]),
            Response = compose_response_xml(UidBin, PlatformId, ReplyText),
            error_logger:info_msg("Response:~ts~n", [Response]),
            Response
    end.

-spec pending_text(Module, Function, Args) -> string() when
    Module :: atom(),
    Function :: atom(),
    Args :: [term()].
pending_text(Module, Function, Args) ->
    Self = self(),
    spawn_link(Module, Function, [Self] ++ Args),
    receive
        {execed, Self, ReturnText} ->
            ReturnText
    end.

-spec return_text(DispatcherPid, ReturnText) -> no_return() when
    ReturnText :: string(),
    DispatcherPid :: pid().
return_text(DispatcherPid, ReturnText) ->
    DispatcherPid ! {execed, DispatcherPid, ReturnText}.

-spec compose_response_xml(Uid, PlatformId, Content) -> binary() when
    Uid :: term(),
    PlatformId :: term(),
    Content :: term().
compose_response_xml(Uid, PlatformId, Content) ->
    case Content of
        ?EMPTY_RESPONSE ->
            ?EMPTY_RESPONSE;
        Response ->
            ResponseList = lists:flatten([<<"<xml><Content><![CDATA[">>, Response, <<"]]></Content><ToUserName><![CDATA[">>, Uid, <<"]]></ToUserName><FromUserName><![CDATA[">>, PlatformId, <<"]]></FromUserName><CreateTime>">>, integer_to_binary(timestamp()), <<"</CreateTime><MsgType><![CDATA[text]]></MsgType></xml>">>]),
            error_logger:info_msg("ResponseList:~p~n", [ResponseList]),
            list_to_binary(ResponseList)
    end.

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
-spec parse_xml_request(Req) -> Result when
    Req :: cowboy_req:req(),
    Result :: parse_failed | map().
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

-spec unmarshall_params(SrcList, ParamsMap) -> map() when
    SrcList :: list(),
    ParamsMap :: map().
unmarshall_params([], ParamsMap) ->
    ParamsMap;
unmarshall_params([{Key, [], [Value]} | Tail], ParamsMap) ->
    unmarshall_params(Tail, maps:put(list_to_atom(Key), unicode:characters_to_binary(string:strip(Value)), ParamsMap)).

-spec concat_param_content(SrcList, ConcatedParamContent) -> list() when
    SrcList :: list(),
    ConcatedParamContent :: list().
concat_param_content([], ConcatedParamContent) ->
    lists:reverse(ConcatedParamContent);
concat_param_content([{_, ParamContent} | Tail], ConcatedParamContent) ->
    concat_param_content(Tail, [ParamContent | ConcatedParamContent]).

-spec timestamp() -> integer().
timestamp() ->
    {Hour, Minute, _} = os:timestamp(),
    Hour * 1000000 + Minute.