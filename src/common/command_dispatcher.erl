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
-define(WECHAT_TOKEN, <<"wechat_mud">>).

-type get_param() :: string() | binary(). % generic string % generic binary
-type post_param() :: binary(). % generic binary
-type command() :: binary(). % generic binary

-record(wechat_get_params, {
    signature :: get_param(),
    timestamp :: get_param(),
    nonce :: get_param(),
    echostr :: get_param()
}).

-record(wechat_post_params, {
    'Content' :: post_param(),
    'CreateTime' :: post_param(),
    'FromUserName' :: post_param(),
    'MsgId' :: post_param(),
    'MsgType' :: post_param(),
    'ToUserName' :: post_param(),
    'Event' :: post_param(),
    'EventKey' :: post_param()
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
-spec start(Req) -> iodata() when
    Req :: cowboy_req:req().
start(Req) ->
    case cowboy_req:qs(Req) of
        <<>> ->
            IsWechatDebug = common_server:is_wechat_debug(),
            case IsWechatDebug of
                true ->
                    process_request(Req);
                false ->
                    error_logger:error_msg("Validation params empty~n", []),
                    ?EMPTY_CONTENT
            end;
        HeaderParams ->
            #wechat_get_params{signature = Signature, timestamp = TimeStamp, nonce = Nonce, echostr = EchoStr} = gen_get_params(HeaderParams),
            ValidationParams = [Signature, TimeStamp, Nonce],
            case validate_signature(ValidationParams) of
                true ->
                    case EchoStr of
                        undefined ->
                            process_request(Req);
                        _EchoStr ->
                            error_logger:info_msg("Connectivity success~n", []),
                            EchoStr
                    end;
                false ->
                    ?EMPTY_CONTENT
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
    ReturnContent :: [nls_server:value()] | no_response.
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
        1000 ->
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
    ReturnContent :: binary() | [binary()],
    DispatcherPid :: pid().
return_content(DispatcherPid, ReturnContent) ->
    DispatcherPid ! {execed, DispatcherPid, ReturnContent},
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
validate_signature([OriginalSignatureBin | ParamList] = OriginalParams) ->
    SortedParamContent = lists:sort([?WECHAT_TOKEN | ParamList]),
    GeneratedSignature = string:to_lower(sha1:hexstring(SortedParamContent)),
    Signature = binary_to_list(OriginalSignatureBin),
    case GeneratedSignature == Signature of
        true ->
            true;
        false ->
            error_logger:error_msg("Validation signature failed:~nParamMap:~p~nGSignature:~p~nOSignature:~p~n", [OriginalParams, GeneratedSignature, Signature]),
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
%% 2. Check if player_fsm exists, if so, it indicates that user has already
%% logged in and then execute process function directly, otherwise go to step 5.
%%
%% 3. Check if register_fsm exists, if so, it indicates that user is in
%% registration procedure and then process registration with user input,
%% otherse go to step 6.
%%
%% 4. Check if user is registered, if so, go to step 5, otherwise starts
%% registration procedure by spawning register_fsm.
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
-spec process_request(Req) -> FormattedResponseContent when
    Req :: cowboy_req:req(),
    FormattedResponseContent :: binary().
process_request(Req) ->
    case parse_xml_request(Req) of
        parse_failed ->
            error_logger:info_msg("Parse xml request failed:~tp~n", [Req]),
            ?EMPTY_CONTENT;
        #wechat_post_params{
            'Content' = RawInputBin,
            'ToUserName' = PlatformId,
            'FromUserName' = UidBin
        } = ReqParams ->
            error_logger:info_msg("User input:~tp~n", [RawInputBin]),

            Uid = binary_to_atom(UidBin, utf8),
            {RawInput, FuncExec} = gen_action_from_message_type(ReqParams),
            ReturnContent =
                case whereis(Uid) of % login_server:is_uid_logged_in(Uid)
                    undefined ->
                        case whereis(register_fsm:register_server_name(Uid)) of % login_server:is_in_registration(Uid)
                            undefined ->
                                case login_server:is_uid_registered(Uid) of
                                    false ->
                                        pending_content(login_server, register_uid, [Uid]);
                                    true ->
                                        if
                                            <<"login">> == RawInput orelse <<"rereg">> == RawInput orelse subscribe == RawInput ->
                                                FuncExec(Uid);
                                            true ->
                                                nls_server:get_nls_content([{nls, please_login}], zh)
                                        end
                                end;
                            _RegisterPid ->
                                if
                                    unsubscribe == RawInput ->
                                        register_fsm:stop(Uid),
                                        no_response;
                                    true ->
                                        pending_content(register_fsm, input, [Uid, RawInput])
                                end
                        end;
                    _PlayerPid ->
                        FuncExec(Uid)
                end,

            Response =
                case ReturnContent of
                    no_response ->
                        <<>>;
                    _ReturnContent ->
                        try
                            ReturnContentBinary = list_to_binary(lists:flatten(elib:remove_last_newline(ReturnContent))),
                            spawn(elib, pp, [ReturnContentBinary]),
                            compose_xml_response(UidBin, PlatformId, ReturnContentBinary)
                        catch
                            Type:Reason ->
                                error_logger:error_msg("Invalid Content:~p~n", [ReturnContent]),
                                error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
                                <<>>
                        end
                end,
            Response
    end.

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
                            nls_server:get_nls_content([{nls, welcome_back}], zh)
                        end};
                <<"unsubscribe">> ->
                    {unsubscribe,
                        fun(Uid) ->
                            handle_input(Uid, <<"logout">>, []),
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
            [ModuleNameBin | RawCommandArgs] = binary:split(RawInput, <<" ">>),
            {RawInput,
                fun(Uid) ->
                    handle_input(Uid, ModuleNameBin, RawCommandArgs)
                end};
        _MsgType ->
            {<<>>,
                fun(Uid) ->
                    nls_server:get_nls_content([{nls, message_type_not_support}], player_fsm:get_lang(Uid))
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
-spec handle_input(Uid, ModuleNameBin, RawCommandArgs) -> ReturnContent when
    Uid :: player_fsm:uid(),
    ModuleNameBin :: binary(),
    RawCommandArgs :: [binary()],
    ReturnContent :: [nls_server:value()].
handle_input(Uid, ModuleNameBin, RawCommandArgs) ->
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
            Args = [Uid | CommandArgs],

            ModuleName:module_info(), % call module_info in order to make function_exported works
            case erlang:function_exported(ModuleName, exec, Arity + 2) of
                true ->
                    pending_content(ModuleName, exec, Args);
                false ->
                    nls_server:get_nls_content([{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(binary_to_list(RawModuleName) ++ "_help")}], player_fsm:get_lang(Uid))
            end;

        invalid_command ->
            nls_server:get_nls_content([{nls, invalid_command}, ModuleNameBin], player_fsm:get_lang(Uid))
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called after content is returned from pending_content/3,
%% it construct xml response only when the return content is not empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_xml_response(UidBin, PlatformIdBin, ContentBin) -> XmlContent when
    UidBin :: binary(),
    PlatformIdBin :: binary(),
    ContentBin :: binary(),
    XmlContent :: binary().
compose_xml_response(UidBin, PlatformIdBin, ContentBin) ->
    XmlContentList = [<<"<xml><Content><![CDATA[">>,
        ContentBin,
        <<"]]></Content><ToUserName><![CDATA[">>,
        UidBin,
        <<"]]></ToUserName><FromUserName><![CDATA[">>,
        PlatformIdBin,
        <<"]]></FromUserName><CreateTime>">>,
        integer_to_binary(elib:timestamp()),
        <<"</CreateTime><MsgType><![CDATA[text]]></MsgType></xml>">>],

    list_to_binary(XmlContentList).

%%--------------------------------------------------------------------
%% @doc
%% This function converts raw xml payload to params map.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_xml_request(Req) -> ReqParams when
    Req :: cowboy_req:req(),
    ReqParams :: #wechat_post_params{} | parse_failed.
parse_xml_request(Req) ->
    {ok, Message, _UpdatedReq} = cowboy_req:body(Req),
    case Message of
        <<>> ->
            parse_failed;
        _Message ->
            {ok, {"xml", [], Params}, _UpdatedOptions} = erlsom:simple_form(Message),
            unmarshall_params(Params, #wechat_post_params{})
    end.

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
    gen_get_params(size(HeaderParams) - 1, HeaderParams, #{}).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for gen_get_params/1.
%% @see gen_get_params/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_get_params(Pos, Bin, AccParamsMap) -> Params when
    Pos :: integer(), % generic integer
    Bin :: binary(),
    AccParamsMap :: map(), % generic map
    Params :: #wechat_get_params{}.
gen_get_params(
    -1,
    _Bin,
    #{
        signature := Signature,
        timestamp := TimeStamp,
        nonce := Nonce
    } = ParamsMap
) ->
    {wechat_get_params, Signature, TimeStamp, Nonce, maps:get(echostr, ParamsMap, undefined)};
gen_get_params(Pos, Bin, ParamsMap) ->
    {ValueBin, CurPosByValue} = gen_get_param_value(binary:at(Bin, Pos), [], Pos - 1, Bin),
    {KeyBin, CurPosByKey} = gen_req_param_key(binary:at(Bin, CurPosByValue), [], CurPosByValue - 1, Bin),
    gen_get_params(CurPosByKey, Bin, maps:put(binary_to_atom(KeyBin, unicode), ValueBin, ParamsMap)).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request param keys.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_req_param_key(CurByte, KeyBinList, Pos, SrcBin) -> {KeyBin, CurPos} when
    CurByte :: byte(),
    KeyBinList :: [CurByte],
    Pos :: integer(), % generic integer
    SrcBin :: binary(),
    KeyBin :: SrcBin,
    CurPos :: Pos.
gen_req_param_key($&, KeyBinList, Pos, _SrcBin) ->
    {list_to_binary(KeyBinList), Pos};
gen_req_param_key(CurByte, KeyBinList, -1, _SrcBin) ->
    {list_to_binary([CurByte | KeyBinList]), -1};
gen_req_param_key(CurByte, KeyBinList, Pos, SrcBin) ->
    gen_req_param_key(binary:at(SrcBin, Pos), [CurByte | KeyBinList], Pos - 1, SrcBin).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request param values.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_get_param_value(CurByte, ValueBinList, Pos, SrcBin) -> {ValueBin, CurPos} when
    CurByte :: byte(),
    ValueBinList :: [CurByte],
    Pos :: integer(), % generic integer
    SrcBin :: binary(),
    ValueBin :: SrcBin,
    CurPos :: Pos.
gen_get_param_value($=, ValueBinList, Pos, _SrcBin) ->
    {list_to_binary(ValueBinList), Pos};
gen_get_param_value(CurByte, ValueBinList, Pos, SrcBin) ->
    gen_get_param_value(binary:at(SrcBin, Pos), [CurByte | ValueBinList], Pos - 1, SrcBin).

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
        Type:Reason ->
            ok = player_fsm:response_content(Uid, [{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(atom_to_list(Module) ++ "_help")}], DispatcherPid),
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
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
is_command_exist(<<"perform">>) -> true;
is_command_exist(<<"attack">>) -> true;
is_command_exist(<<"ask">>) -> true;
is_command_exist(_InvalidCommand) -> false.