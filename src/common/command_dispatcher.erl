%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Command dispatcher module. All wechat requests firstly mapped to
%%% this module and dispacthes to corresponding server or module for
%%% furthur process, and reply processed contents back to user.
%%%
%%% @end
%%% Created : 19. Aug 2015 10:06 PM
%%%-------------------------------------------------------------------
-module(command_dispatcher).
-author("Shuieryin").

%% API
-export([
    start/1,
    pending_content/3,
    return_content/2
]).

-define(MAX_CONTENT_SIZE, 2048).
-define(EMPTY_CONTENT, <<>>).
-define(AFFAIR_INPUT_DIGIT_SIZE, <<"2">>).
-define(Get_lang(Uid), player_statem:get_lang(Uid)).

-type get_param() :: string() | binary(). % generic string % generic binary
-type post_param() :: binary(). % generic binary
-type command() :: binary(). % generic binary
-type return_content() :: nls_server:value() | {image, binary()} | [nls_server:value()] | no_response.

-record(wechat_get_params, {
    signature :: get_param() | undefined,
    timestamp :: get_param(),
    nonce :: get_param(),
    openid :: get_param(),
    encrypt_type :: get_param() | undefined,
    msg_signature :: get_param() | undefined,
    echostr :: get_param() | undefined
}).

-record(wechat_post_params, {
    'Content' :: post_param() | undefined,
    'CreateTime' :: post_param() | undefined,
    'FromUserName' :: post_param() | undefined,
    'MsgId' :: post_param() | undefined,
    'MsgType' :: post_param() | undefined,
    'ToUserName' :: post_param() | undefined,
    'Event' :: post_param() | undefined,
    'EventKey' :: post_param() | undefined,
    'Encrypt' :: post_param() | undefined,
    'AgentID' :: post_param() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% All wechat requests start here. This function does the followings:
%%
%% 1. extract parameters from request, go to step 2 if empty, otherwise go to
%% step 3.
%%
%% 2. if debug mode is on, process request, otherwise print error message
%% and reply empty response. If wechat debug mode is on, the signature validation
%% will be skipped by requests sent from wechat debug tool (http://mp.weixin.qq.com/debug).
%%
%% 3. extract params and validate signature, if passed, go to step 4, otherwise
%% reply empty response
%%
%% 4. check if echostr exists, if so, such request is connectivity request
%% and reply echostr param, otherwise process request.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Req) -> {Reply, UpdatedReq} when
    Req :: cowboy_req:req(),
    Reply :: iodata(),
    UpdatedReq :: Req.
start(Req) ->
    case cowboy_req:qs(Req) of
        ?EMPTY_CONTENT ->
            IsWechatDebug = common_server:is_wechat_debug(),
            case IsWechatDebug of
                true ->
                    process_request(Req, undefined);
                false ->
                    error_logger:error_msg("Validation params empty~n", []),
                    {?EMPTY_CONTENT, Req}
            end;
        HeaderParams ->
            #wechat_get_params{
                signature = Signature,
                timestamp = Timestamp,
                nonce = Nonce,
                echostr = EchoStr,
                openid = OpenId,
                msg_signature = MsgSignature
            } = gen_get_params(HeaderParams),
            [{DecodedAESKey, WechatToken, AppId}] = ets:tab2list(misc_table),
            PassFirstValidation =
                case Signature of
                    undefined ->
                        true;
                    _HasSignature ->
                        ValidationParams = [Signature, WechatToken, Timestamp, Nonce],
                        validate_signature(ValidationParams)
                end,
            case PassFirstValidation of
                true ->
                    EncryptParams =
                        case MsgSignature of
                            undefined ->
                                undefined;
                            _HasMsgSignature ->
                                {OpenId, [MsgSignature, WechatToken, Timestamp, Nonce], DecodedAESKey, AppId, EchoStr}
                        end,
                    process_request(Req, EncryptParams);
                false ->
                    {?EMPTY_CONTENT, Req}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function spawn_link a command module and waits its return
%% content, if error occurs in the spawn process the whole request
%% journey halted by replying NO message to user and this is
%% expected behaviour.
%%
%% @end
%%--------------------------------------------------------------------
-spec pending_content(Module, Function, Args) -> ReturnContent when
    Module :: module(),
    Function :: atom(), % generic atom
    Args :: [term()], % generic term
    ReturnContent :: return_content().
pending_content(Module, Function, Args) ->
    Self = self(),
    FunctionArgs = [Self | Args],
    spawn(
        fun() ->
            execute_command(Module, Function, FunctionArgs)
        end
    ),
    receive
        {execed, Self, ReturnContent} ->
            ReturnContent
    after
        2000 ->
            no_response
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send back processed results to pid from function pending_content/3.
%% @see pending_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec return_content(DispatcherPid, ReturnContent) -> ok when
    ReturnContent :: return_content(),
    DispatcherPid :: pid().
return_content(DispatcherPid, ReturnContent) ->
    DispatcherPid ! {execed, DispatcherPid, ReturnContent},
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generate signature.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_signature(ParamList :: [binary()]) -> string().
generate_signature(ParamList) ->
    SortedParamContent = lists:sort(ParamList),
    string:to_lower(sha1:hexstring(SortedParamContent)).

%%--------------------------------------------------------------------
%% @doc
%% Extract params and generate siganture first, compare the generated
%% signature with the signature from request, and return the result.
%%
%% Validate signature based on wechat doc below:
%% http://mp.weixin.qq.com/wiki/17/2d4265491f12608cd170a95559800f2d.html
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_signature(OriginalParams) -> IsValidSignature when
    OriginalParams :: [binary()], % generic binary
    IsValidSignature :: boolean().
validate_signature([OriginalSignatureBin | ParamList]) ->
    GeneratedSignature = generate_signature(ParamList),
    Signature = binary_to_list(OriginalSignatureBin),
    case GeneratedSignature == Signature of
        true ->
            true;
        false ->
            error_logger:error_msg("Validation signature failed:~nParamList:~p~nGSignature:~p~nOSignature:~p~n", [ParamList, GeneratedSignature, Signature]),
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called after signature validation is passed or when
%% debug mode is on.
%%
%% This function parse request params from xml to map first, if failed,
%% print error message and return empty content, otherwise process the
%% followings:
%%
%% 1. Extract raw inputs and generate process function from private
%% function gen_action_from_message_type/2.
%%
%% 2. Check if player_statem exists, if so, it indicates that user has already
%% logged in and then execute process function directly, otherwise go to step 5.
%%
%% 3. Check if register_statem exists, if so, it indicates that user is in
%% registration procedure and then process registration with user input,
%% otherse go to step 6.
%%
%% 4. Check if user is registered, if so, go to step 5, otherwise starts
%% registration procedure by spawning register_statem.
%%
%% 5. If raw input is empty, return empty content, otherwise go to step 6.
%%
%% 6. If raw input is
%%          "login" - process login procedure
%%          "rereg" - process rereg procedure
%% otherwise reply message by reminding user to login.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_request(Req, EncryptParams) -> {FormattedResponseContent, UpdatedReq} when
    Req :: cowboy_req:req(),
    EncryptParams :: {OpenId, NextValidationParams, DecodedAESKey, AppId, EchoStr} | undefined,
    OpenId :: get_param(),
    NextValidationParams :: [get_param()],
    DecodedAESKey :: get_param(),
    AppId :: get_param(),
    EchoStr :: get_param(),
    FormattedResponseContent :: binary(),
    UpdatedReq :: Req.
process_request(Req, EncryptParams) ->
    {ReqParams, UpdatedReq} = parse_xml_request(Req),
%%    error_logger:info_msg("ReqParams:~p~n", [ReqParams]),
    {FinalReqParams, DecodedAESKey, AppId, EchoStr} =
        case EncryptParams of
            undefined ->
                {ReqParams, undefined, undefined, undefined};
            {_RawOpenId, NextValidationParams, RawDecodedAESKey, RawAppId, RawEchoStr} ->
                #wechat_post_params{
                    'ToUserName' = RawPlatformId,
                    'Encrypt' = EncryptedContent
                } = ReqParams,
                case RawEchoStr of
                    undefined ->
                        case validate_signature(NextValidationParams ++ [EncryptedContent]) of
                            true ->
                                DecryptedContent = decrypt(EncryptedContent, RawDecodedAESKey, RawAppId),
                                {ok, {"xml", [], Params}, _UpdatedOptions} = erlsom:simple_form(DecryptedContent),
                                #wechat_post_params{
                                    'ToUserName' = ToUserName
                                } = RawReqParams = unmarshall_params(Params, #wechat_post_params{}),
                                ToUserName = RawPlatformId, % assertion
                                {RawReqParams, RawDecodedAESKey, RawAppId, RawEchoStr};
                            _Other ->
                                {parse_failed, undefined, undefined, undefined}
                        end;
                    _Connectivity ->
                        UriDecodedEchoStr = http_uri:decode(RawEchoStr),
                        case validate_signature(NextValidationParams ++ [UriDecodedEchoStr]) of
                            true ->
                                DecryptedEchoStr = decrypt(UriDecodedEchoStr, RawDecodedAESKey, RawAppId),
                                {undefined, undefined, undefined, DecryptedEchoStr};
                            _Other ->
                                {parse_failed, undefined, undefined, undefined}
                        end
                end
        end,

%%    error_logger:info_msg("FinalReqParams:~p~n", [FinalReqParams]),

    FinalReply =
        case EchoStr of
            undefined ->
                RawFinalReply =
                    case FinalReqParams of
                        parse_failed ->
                            error_logger:info_msg("Parse xml request failed:~tp~n", [Req]),
                            ?EMPTY_CONTENT;
                        #wechat_post_params{
                            'Content' = RawInputBin,
                            'ToUserName' = PlatformId,
                            'FromUserName' = UidBin
                            %'MsgId' = MsgId
                        } = FinalReqParams ->
                            error_logger:info_msg("User input:~tp~n", [RawInputBin]),

                            Uid = binary_to_atom(UidBin, utf8),
                            {RawInput, FuncExec} = gen_action_from_message_type(FinalReqParams),
                            ReturnContent =
                                case whereis(Uid) of % login_server:is_uid_logged_in(Uid)
                                    undefined ->
                                        case whereis(register_statem:register_server_name(Uid)) of % login_server:is_in_registration(Uid)
                                            undefined ->
                                                case login_server:is_uid_registered(Uid) of
                                                    false ->
                                                        pending_content(login_server, register_uid, [Uid]);
                                                    true ->
                                                        if
                                                            <<"login">> == RawInput orelse <<"rereg">> == RawInput orelse subscribe == RawInput ->
                                                                FuncExec(Uid);
                                                            true ->
                                                                nls_server:nls_content([{nls, please_login}], zh)
                                                        end
                                                end;
                                            _RegisterPid ->
                                                if
                                                    unsubscribe == RawInput ->
                                                        register_statem:stop(Uid),
                                                        no_response;
                                                    true ->
                                                        pending_content(register_statem, input, [Uid, RawInput])
                                                end
                                        end;
                                    _PlayerPid ->
                                        FuncExec(Uid)
                                end,

                            Response =
                                try
                                    case ReturnContent of
                                        no_response ->
                                            <<>>;
                                        {image, ImageUrl} ->
                                            compose_image_response(UidBin, PlatformId, ImageUrl);
                                        _ReturnContent ->
                                            ReturnContentBinary = list_to_binary(lists:flatten(elib:remove_last_newline(ReturnContent))),
                                            spawn(elib, pp, [ReturnContentBinary]),
                                            compose_text_response(UidBin, PlatformId, ReturnContentBinary)
                                    end
                                catch
                                    Type:Reason:Stacktrace ->
                                        error_logger:error_msg("Invalid Content:~p~n", [ReturnContent]),
                                        error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, Stacktrace]),
                                        <<>>
                                end,
                            Response
                    end,
                case EncryptParams of
                    undefined ->
                        RawFinalReply;
                    {_OpenId, [_FromMsgSignature, WechatToken, Timestamp, Nonce], DecodedAESKey, AppId, EchoStr} ->
                        EncodedReply = encode(RawFinalReply, DecodedAESKey, AppId),
                        ToMsgSignature = list_to_binary(generate_signature([WechatToken, Timestamp, Nonce, EncodedReply])),
                        ComposedReply = compose_encrypt_response(EncodedReply, ToMsgSignature, Timestamp, Nonce),
%%                            error_logger:info_msg("ComposedReply:~p~n", [ComposedReply]),
                        ComposedReply
                end;
            _HasEchoStr ->
                error_logger:info_msg("Connectivity success~n", []),
                EchoStr
        end,
    {FinalReply, UpdatedReq}.

%%--------------------------------------------------------------------
%% @doc
%% This function genereates action functions according to wechat
%% request type (all reuqest types can be found wehcat doc
%% http://mp.weixin.qq.com/wiki/10/79502792eef98d6e0c6e1739da387346.html).
%%
%% The so far supported wechat request type is as follows:
%% - event
%%      - subscribe
%%              if user registered, return welcome back message by
%%              languauge, otherwise start registration process.
%%      - unsubscribe
%%              logoff user
%%      - other events not supported
%%
%% - text
%%      split user input by separator space, which the first element
%%      is treated as module name and the rest are arguments.
%%
%% - other action types
%%      reply type not supported message
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_action_from_message_type(ReqParams) -> {InputForUnregister, FuncForRegsiter} when
    ReqParams :: #wechat_post_params{},
    InputForUnregister :: binary() | no_reply | subscribe | unsubscribe,
    FuncForRegsiter :: function().
gen_action_from_message_type(
    #wechat_post_params{
        'MsgType' = MsgType,
        'Event' = Event
    } = ReqParams
) ->
    case MsgType of
        <<"event">> ->
            case Event of
                <<"subscribe">> ->
                    {subscribe,
                        fun(_Uid) ->
                            nls_server:nls_content([{nls, welcome_back}], zh)
                        end};
                <<"unsubscribe">> ->
                    {unsubscribe,
                        fun(Uid) ->
                            handle_input(Uid, <<"logout">>),
                            no_response
                        end};
                _Event ->
                    {no_reply,
                        fun(_Uid) ->
                            no_response
                        end}
            end;
        <<"text">> ->
            RawInput = ReqParams#wechat_post_params.'Content',
            {
                RawInput,
                fun(Uid) ->
                    handle_input(Uid, RawInput)
                end
            };
        _MsgType ->
            {<<>>,
                fun(Uid) ->
                    nls_server:nls_content([{nls, message_type_not_support}], ?Get_lang(Uid))
                end}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called only when user has logged in, input is
%% "login" or action type is subscribe. It process as followings:
%%
%% 1. Check if module name exist, if not, check if the "ModuleNameStr"
%% is a direction, if not, throw exception by going to step 3,
%% otherwise go to step 2.
%%
%% 2. Check if module has "exec" function with corresponding arity,
%% if so, process module, otherwise reply message by warning user with
%% the invalid arguments and the commands manual.
%%
%% 3. If exeption is catched, print error log and reply messages by
%% warning user command not exist.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_input(Uid, RawInput) -> ReturnContent when
    Uid :: player_statem:uid(),
    RawInput :: binary(),
    ReturnContent :: [nls_server:value()].
handle_input(Uid, RawInput) ->
    MatchStatus = re:run(RawInput, <<"^[0-9]{1,", ?AFFAIR_INPUT_DIGIT_SIZE/binary, "}">>),
    case MatchStatus of
        nomatch ->
            handle_normal_input(Uid, RawInput);
        {match, _Match} ->
            AffairName = player_statem:affair_name(Uid),
            % error_logger:info_msg("AffairName:~p~n", [AffairName]),
            case AffairName of
                undefined ->
                    handle_normal_input(Uid, RawInput);
                _InAffair ->
                    pending_content(player_statem, handle_affair_input, [Uid, RawInput])
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called after content is returned from pending_content/3,
%% it construct IMAGE response only when the return content is not empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_image_response(UidBin, PlatformIdBin, ImageId) -> XmlContent when
    UidBin :: binary(),
    PlatformIdBin :: binary(),
    ImageId :: binary(),
    XmlContent :: binary().
compose_image_response(UidBin, PlatformIdBin, ImageId) ->
    TimestampBin = integer_to_binary(elib:timestamp()),
    Xml = <<"<xml>
        <ToUserName><![CDATA[", UidBin/binary, "]]></ToUserName>
        <FromUserName><![CDATA[", PlatformIdBin/binary, "]]></FromUserName>
        <CreateTime>", TimestampBin/binary, "</CreateTime>
        <MsgType><![CDATA[image]]></MsgType>
        <Image>
            <MediaId><![CDATA[", ImageId/binary, "]]></MediaId>
        </Image>
    </xml>">>,

    error_logger:info_msg("ImageXmlResponse:~p~n", [Xml]),
    Xml.

%%--------------------------------------------------------------------
%% @doc
%% This function is called after content is returned from pending_content/3,
%% it construct TEXT response only when the return content is not empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_text_response(UidBin, PlatformIdBin, ContentBin) -> XmlContent when
    UidBin :: binary(),
    PlatformIdBin :: binary(),
    ContentBin :: binary(),
    XmlContent :: binary().
compose_text_response(UidBin, PlatformIdBin, ContentBin) ->
    TimestampBin = integer_to_binary(elib:timestamp()),
    <<"<xml><Content><![CDATA[", ContentBin/binary, "]]></Content><ToUserName><![CDATA[", UidBin/binary, "]]></ToUserName><FromUserName><![CDATA[", PlatformIdBin/binary, "]]></FromUserName><CreateTime>", TimestampBin/binary, "</CreateTime><MsgType><![CDATA[text]]></MsgType></xml>">>.


%%--------------------------------------------------------------------
%% @doc
%% This function construct ENCRYPT response only when the return content is not empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_encrypt_response(Encrypt, MsgSignature, Timestamp, Nonce) -> XmlContent when
    Encrypt :: binary(),
    MsgSignature :: binary(),
    Timestamp :: binary(),
    Nonce :: binary(),
    XmlContent :: binary().
compose_encrypt_response(Encrypt, MsgSignature, Timestamp, Nonce) ->
    <<"<xml><Encrypt><![CDATA[", Encrypt/binary, "]]></Encrypt><MsgSignature><![CDATA[", MsgSignature/binary, "]]></MsgSignature><TimeStamp><![CDATA[", Timestamp/binary, "]]></TimeStamp><Nonce><![CDATA[", Nonce/binary, "]]></Nonce></xml>">>.

%%--------------------------------------------------------------------
%% @doc
%% This function converts raw xml payload to params map.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_xml_request(Req) -> {ReqParams, UpdatedReq} when
    Req :: cowboy_req:req(),
    ReqParams :: #wechat_post_params{} | parse_failed,
    UpdatedReq :: Req.
parse_xml_request(Req) ->
    {ok, Message, UpdatedReq} = cowboy_req:read_body(Req),
    ReqParams =
        case Message of
            <<>> ->
                #wechat_post_params{};
            _Message ->
                {ok, {"xml", [], Params}, _UpdatedOptions} = erlsom:simple_form(Message),
                unmarshall_params(Params, #wechat_post_params{})
        end,
    {ReqParams, UpdatedReq}.

%%--------------------------------------------------------------------
%% @doc
%% This function converts below structure to map.
%%
%% This structure is genereated by erlsom:simple_form/1
%% [{"ToUserName", [], [PlatFormId]},
%% {"FromUserName", [], [Uid]},
%% {"CreateTime", [], [CreateTime]},
%% {"MsgType", [], [MsgType]},
%% {"Content", [], [Content]},
%% {"MsgId", [], [MsgId]}]
%%
%% @end
%%--------------------------------------------------------------------
-spec unmarshall_params(SrcList, ParamsRecord) -> FinalParamsRecord when
    SrcList :: [{ParamKey, [], [ParamValue]}],
    ParamKey :: string(),
    ParamValue :: string(),
    ParamsRecord :: #wechat_post_params{},
    FinalParamsRecord :: ParamsRecord.
unmarshall_params([], ParamsRecord) ->
    ParamsRecord;
unmarshall_params([{"Content", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'Content' = unicode:characters_to_binary(string:to_lower(string:strip(ParamValue)))});
unmarshall_params([{"CreateTime", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'CreateTime' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"FromUserName", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'FromUserName' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"MsgId", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'MsgId' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"MsgType", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'MsgType' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"ToUserName", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'ToUserName' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"Event", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'Event' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"EventKey", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'EventKey' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"Encrypt", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'Encrypt' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{"AgentID", [], [ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord#wechat_post_params{'AgentID' = unicode:characters_to_binary(ParamValue)});
unmarshall_params([{_Other, [], [_ParamValue]} | Tail], ParamsRecord) ->
    unmarshall_params(Tail, ParamsRecord).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request params to
%% wechat_get_param record.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_get_params(binary()) -> #wechat_get_params{}.
gen_get_params(HeaderParams) ->
    #{
        timestamp := TimeStamp,
        nonce := Nonce
    } = ParamsMap = elib:gen_get_params(HeaderParams),
    {wechat_get_params,
        maps:get(signature, ParamsMap, undefined),
        TimeStamp,
        Nonce,
        maps:get(openid, ParamsMap, undefined),
        maps:get(encrypt_type, ParamsMap, undefined),
        maps:get(msg_signature, ParamsMap, undefined),
        maps:get(echostr, ParamsMap, undefined)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Return command help content to player before throwm catched
%% exception. This function can be called only within catch clause.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_command(Module, Function, FunctionArgs) -> ok when
    Module :: module(),
    Function :: atom(), % generic atom
    FunctionArgs :: [term()]. % generic term
execute_command(Module, Function, [DispatcherPid, Uid | CommandArgs] = FunctionArgs) ->
    try
        apply(Module, Function, FunctionArgs)
    catch
        Type:Reason:Stacktrace ->
            {ok, _Pid} = player_statem:response_content(Uid, [{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(atom_to_list(Module) ++ "_help")}], DispatcherPid),
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, Stacktrace]),
            throw(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parse the raw command from user input to original command module name.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_raw_command(RawCommand) -> Command when
    RawCommand :: command(),
    Command :: RawCommand.
parse_raw_command(<<"5">>) -> <<"look">>;
parse_raw_command(<<"l">>) -> <<"look">>;
parse_raw_command(Other) -> Other.

%%--------------------------------------------------------------------
%% @doc
%% Checks if command exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_command_exist(CommandBin) -> boolean() when
    CommandBin :: command().
is_command_exist(<<"look">>) -> true;
is_command_exist(<<"lang">>) -> true;
is_command_exist(<<"login">>) -> true;
is_command_exist(<<"logout">>) -> true;
is_command_exist(<<"rereg">>) -> true;
is_command_exist(<<"hp">>) -> true;
is_command_exist(<<"attack">>) -> true;
is_command_exist(<<"ask">>) -> true;
is_command_exist(SpecialHandling) ->
    if
        SpecialHandling == <<"perform">> ->
            true;
        true ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle normal input.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_normal_input(Uid, RawInput) -> ReturnContent :: [nls_server:value()] when
    Uid :: player_statem:uid(),
    RawInput :: binary().
handle_normal_input(Uid, RawInput) ->
    [ModuleNameBin | RawCommandArgs] = binary:split(RawInput, <<" ">>),

    RawModuleName = parse_raw_command(ModuleNameBin),

    CommandInfo =
        case is_command_exist(RawModuleName) of
            true ->
                {binary_to_atom(RawModuleName, utf8), RawCommandArgs};
            false ->
                case direction:parse_direction(RawModuleName) of
                    undefined ->
                        invalid_command;
                    Direction ->
                        direction:module_info(),
                        {direction, [Direction]}
                end
        end,

    case CommandInfo of
        {ModuleName, CommandArgs} ->
            Arity = length(CommandArgs),
            Args = [Uid, RawInput | CommandArgs],

            ModuleName:module_info(), % call module_info in order to make function_exported works
            case erlang:function_exported(ModuleName, exec, Arity + 3) of
                true ->
                    pending_content(ModuleName, exec, Args);
                false ->
                    nls_server:nls_content([{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(binary_to_list(RawModuleName) ++ "_help")}], ?Get_lang(Uid))
            end;

        invalid_command ->
            nls_server:nls_content([{nls, invalid_command}, ModuleNameBin], ?Get_lang(Uid))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Encrypt and encode message.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(FinalReply :: binary(), DecodedAESKey :: binary(), AppId :: binary()) -> EncodedReply :: binary().
encode(FinalReply, DecodedAESKey, AppId) ->
    {BIvec, _BNextIv} = split_binary(DecodedAESKey, 16),
    RandomPrefix = crypto:strong_rand_bytes(16),
    ReplyLen = size(FinalReply),
    LenBin = <<ReplyLen:32>>,
    RawReply = <<RandomPrefix/binary, LenBin/binary, FinalReply/binary, AppId/binary>>,
    RawPackedReply = cm:pkcs7pad(RawReply),
    EncryptedReply = crypto:block_encrypt(aes_cbc256, DecodedAESKey, BIvec, RawPackedReply),
    base64:encode(EncryptedReply).

%%--------------------------------------------------------------------
%% @doc
%% Decoded and decrypt message.
%%
%% @end
%%--------------------------------------------------------------------
-spec decrypt(EncryptedContent :: binary(), DecodedAESKey :: binary(), AppId :: binary()) -> DecryptedContent :: binary().
decrypt(EncryptedContent, DecodedAESKey, AppId) ->
    32 = size(DecodedAESKey), % assertion
    {Ivec, _NextIv} = split_binary(DecodedAESKey, 16),
    DecodedContent = base64:mime_decode(EncryptedContent),
    OriDecryptedContent = crypto:block_decrypt(aes_cbc256, DecodedAESKey, Ivec, DecodedContent),
    {_Other, RawDecryptedContent} = split_binary(OriDecryptedContent, 20),
    RawUnpaadedContent = cm:pkcs7unpad(RawDecryptedContent),
    AppIdSize = byte_size(AppId),
    ContentSize = byte_size(RawUnpaadedContent) - AppIdSize,
    <<FinalContent:ContentSize/binary, AppId:AppIdSize/binary>> = RawUnpaadedContent,
    FinalContent.