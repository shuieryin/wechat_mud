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
-export([start/1,
    pending_content/3,
    return_content/2]).

-define(MAX_CONTENT_SIZE, 2048).
-define(EMPTY_CONTENT, <<>>).
-define(WECHAT_TOKEN, <<"collinguo">>).

-export_type([uid_profile/0]).

-type uid_profile() :: #{uid => atom(), gender => male | female, born_month => 1..12, scene => atom(), lang => atom(), register_time => pos_integer()}.
-type command() :: '5' | l.

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
-spec start(Req) -> string() when
    Req :: cowboy_req:req().
start(Req) ->
    case cowboy_req:qs(Req) of
        <<>> ->
            IsWechatDebug = common_server:is_wechat_debug(),
            case IsWechatDebug of
                true ->
                    process_request(Req);
                _ ->
                    error_logger:error_msg("Header params empty~n", []),
                    ?EMPTY_CONTENT
            end;
        HeaderParams ->
            ParamsMap = gen_req_params_map(size(HeaderParams) - 1, HeaderParams, #{}),
%%             error_logger:info_msg("ParamsMap:~p~n", [ParamsMap]),
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
    Module :: atom(),
    Function :: atom(),
    Args :: [term()],
    ReturnContent :: [binary()].
pending_content(Module, Function, Args) ->
    Self = self(),
    FunctionArgs = [Self | Args],
    spawn(fun() ->
        execute_command(Module, Function, FunctionArgs)
    end),
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
-spec validate_signature(OriginalParamMap) -> IsValidSignature when
    OriginalParamMap :: map(),
    IsValidSignature :: boolean().
validate_signature(#{signature := OriginalSignatureStr} = OriginalParamMap) ->
    ParamList = maps:to_list(maps:without([signature], OriginalParamMap)),
    GeneratedSignature = generate_signature(ParamList),
    Signature = binary_to_list(OriginalSignatureStr),
    case GeneratedSignature == Signature of
        true ->
            true;
        _ ->
            error_logger:error_msg("Validation signature failed:~nParamMap:~p~nGSignature:~p~nOSignature:~p~n", [OriginalParamMap, GeneratedSignature, Signature]),
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Signature generation based on wechat doc below:
%% http://mp.weixin.qq.com/wiki/17/2d4265491f12608cd170a95559800f2d.html
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_signature(OriginParamList) -> SignatureStr when
    OriginParamList :: list(),
    SignatureStr :: string().
generate_signature(OriginParamList) ->
    ConcatedParamContent = [?WECHAT_TOKEN | [Param || {_, Param} <- OriginParamList]],
    SortedParamContent = lists:sort(ConcatedParamContent),
    string:to_lower(sha1:hexstring(SortedParamContent)).

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
        #{'MsgType' := MsgType, 'ToUserName' := PlatformId, 'FromUserName' := UidBin} = ReqParamsMap ->
            error_logger:info_msg("ReqParamsMap:~tp~n", [ReqParamsMap]),

            Uid = binary_to_atom(UidBin, utf8),
            {RawInput, FuncForRegsiteredUser} = gen_action_from_message_type(MsgType, ReqParamsMap),
            ReturnContent =
                case whereis(Uid) of % login_server:is_uid_logged_in(Uid)
                    undefined ->
                        case whereis(register_fsm:fsm_server_name(Uid)) of % login_server:is_in_registration(Uid)
                            undefined ->
                                case login_server:is_uid_registered(Uid) of
                                    false ->
                                        pending_content(login_server, register_uid, [Uid]);
                                    _ ->
                                        if
                                            <<"login">> == RawInput orelse <<"rereg">> == RawInput orelse subscribe == RawInput ->
                                                FuncForRegsiteredUser(Uid);
                                            true ->
                                                nls_server:get_nls_content(commands, [{nls, please_login}], zh)
                                        end
                                end;
                            _ ->
                                pending_content(register_fsm, input, [Uid, RawInput])
                        end;
                    _ ->
                        FuncForRegsiteredUser(Uid)
                end,

            Response =
                case ReturnContent of
                    no_response ->
                        <<>>;
                    _ ->
                        try
                            ReturnContentBinary = list_to_binary(lists:flatten(common_api:remove_last_newline(ReturnContent))),
                            error_logger:info_msg("ReplyContent:~tp~n", [ReturnContentBinary]),
                            compose_xml_response(UidBin, PlatformId, ReturnContentBinary)
                        catch
                            Type:Reason ->
                                error_logger:error_msg("Invalid Content:~p~n", [ReturnContent]),
                                error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
                                <<>>
                        end
                end,

%%             error_logger:info_msg("Response:~ts~n", [binary_to_list(Response)]),
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
-spec gen_action_from_message_type(MsgType, ReqParamsMap) -> {InputForUnregister, FuncForRegsiter} when
    MsgType :: binary(),
    ReqParamsMap :: map(),
    InputForUnregister :: binary() | no_reply | subscribe | unsubscribe,
    FuncForRegsiter :: function().
gen_action_from_message_type(MsgType, ReqParamsMap) ->
    case binary_to_atom(MsgType, utf8) of
        event ->
            Event = binary_to_atom(maps:get('Event', ReqParamsMap), utf8),
            case Event of
                subscribe ->
                    {subscribe, fun(_Uid) ->
                        nls_server:get_nls_content(commands, [{nls, welcome_back}], zh)
                    end};
                unsubscribe ->
                    {unsubscribe, fun(Uid) ->
                        handle_input(Uid, <<"logout">>, [])
                    end};
                _ ->
                    {no_reply, fun(_Uid) ->
                        ?EMPTY_CONTENT
                    end}
            end;
        text ->
            % _MsgId = maps:get('MsgId', ReqParamsMap),
            RawInput = maps:get('Content', ReqParamsMap),
            [ModuleNameBin | RawCommandArgs] = binary:split(RawInput, <<" ">>),
            {RawInput, fun(Uid) ->
                handle_input(Uid, ModuleNameBin, RawCommandArgs)
            end};
        _ ->
            {<<>>, fun(Uid) ->
                nls_server:get_nls_content(commands, [{nls, message_type_not_support}], player_fsm:get_lang(Uid))
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
    Uid :: atom(),
    ModuleNameBin :: binary(),
    RawCommandArgs :: [binary()],
    ReturnContent :: [term()].
handle_input(Uid, ModuleNameBin, RawCommandArgs) ->
    try
        RawModuleName = parse_raw_command(list_to_atom(string:to_lower(binary_to_list(ModuleNameBin)))),
        {ModuleName, CommandArgs} =
            case common_api:is_module_exists(RawModuleName) of
                true ->
                    {RawModuleName, RawCommandArgs};
                _ ->
                    case direction:parse_direction(RawModuleName) of
                        undefined ->
                            throw(not_direction);
                        Direction ->
                            direction:module_info(),
                            {direction, [Direction]}
                    end
            end,

        Arity = length(CommandArgs),
        Args = [Uid | CommandArgs],

        case erlang:function_exported(ModuleName, exec, Arity + 2) of
            true ->
                pending_content(ModuleName, exec, Args);
            _ ->
                nls_server:get_nls_content(commands, [{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(binary_to_list(ModuleNameBin) ++ "_help")}], player_fsm:get_lang(Uid))
        end
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
            nls_server:get_nls_content(commands, [{nls, invalid_command}, ModuleNameBin], player_fsm:get_lang(Uid))
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called after content is returned from pending_content/3,
%% it construct xml response only when the return content is not empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_xml_response(Uid, PlatformId, ContentBinary) -> XmlContent when
    Uid :: term(),
    PlatformId :: term(),
    ContentBinary :: binary(),
    XmlContent :: binary().
compose_xml_response(Uid, PlatformId, ContentBinary) ->
    XmlContentList = [<<"<xml><Content><![CDATA[">>,
        ContentBinary,
        <<"]]></Content><ToUserName><![CDATA[">>,
        Uid,
        <<"]]></ToUserName><FromUserName><![CDATA[">>,
        PlatformId,
        <<"]]></FromUserName><CreateTime>">>,
        integer_to_binary(common_api:timestamp()),
        <<"</CreateTime><MsgType><![CDATA[text]]></MsgType></xml>">>],

    list_to_binary(XmlContentList).

%%--------------------------------------------------------------------
%% @doc
%% This function converts raw xml payload to params map.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_xml_request(Req) -> Result when
    Req :: cowboy_req:req(),
    Result :: parse_failed | map().
parse_xml_request(Req) ->
    {ok, Message, _} = cowboy_req:body(Req),
%%     error_logger:info_msg("Message:~p~n", [Message]),
    case Message of
        <<>> ->
            parse_failed;
        _ ->
            {ok, {"xml", [], Params}, _} = erlsom:simple_form(Message),
            unmarshall_params(Params, #{})
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
-spec unmarshall_params(SrcList, ParamsMap) -> FinalParamsMap when
    SrcList :: [{ParamKey, [], [ParamValue]}],
    ParamKey :: string(),
    ParamValue :: string(),
    ParamsMap :: map(),
    FinalParamsMap :: map().
unmarshall_params([], ParamsMap) ->
    ParamsMap;
unmarshall_params([{ParamKey, [], [ParamValue]} | Tail], ParamsMap) ->
    unmarshall_params(Tail, maps:put(list_to_atom(ParamKey), unicode:characters_to_binary(string:strip(ParamValue)), ParamsMap)).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request params to params map.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_req_params_map(Pos, Bin, ParamsMap) -> map() when
    Pos :: integer(),
    Bin :: binary(),
    ParamsMap :: map().
gen_req_params_map(-1, _, ParamsMap) ->
    ParamsMap;
gen_req_params_map(Pos, Bin, ParamsMap) ->
    {ValueBin, CurPosByValue} = gen_req_param_value(binary:at(Bin, Pos), [], Pos - 1, Bin),
    {KeyBin, CurPosByKey} = gen_req_param_key(binary:at(Bin, CurPosByValue), [], CurPosByValue - 1, Bin),
    gen_req_params_map(CurPosByKey, Bin, maps:put(binary_to_atom(KeyBin, unicode), ValueBin, ParamsMap)).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request param keys.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_req_param_key(CurByte, KeyBinList, Pos, SrcBin) -> {binary(), integer()} when
    CurByte :: byte(),
    KeyBinList :: [byte()],
    Pos :: integer(),
    SrcBin :: binary().
gen_req_param_key($&, KeyBinList, Pos, _) ->
    {list_to_binary(KeyBinList), Pos};
gen_req_param_key(CurByte, KeyBinList, -1, _) ->
    {list_to_binary([CurByte | KeyBinList]), -1};
gen_req_param_key(CurByte, KeyBinList, Pos, SrcBin) ->
    gen_req_param_key(binary:at(SrcBin, Pos), [CurByte | KeyBinList], Pos - 1, SrcBin).

%%--------------------------------------------------------------------
%% @doc
%% This function generates request raw request param values.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_req_param_value(CurByte, ValueBinList, Pos, SrcBin) -> {binary(), integer()} when
    CurByte :: byte(),
    ValueBinList :: [byte()],
    Pos :: integer(),
    SrcBin :: binary().
gen_req_param_value($=, ValueBinList, Pos, _) ->
    {list_to_binary(ValueBinList), Pos};
gen_req_param_value(CurByte, ValueBinList, Pos, SrcBin) ->
    gen_req_param_value(binary:at(SrcBin, Pos), [CurByte | ValueBinList], Pos - 1, SrcBin).

%%--------------------------------------------------------------------
%% @doc
%% Return command help content to player before throwm catched
%% exception. This function can be called only within catch clause.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_command(Module, Function, FunctionArgs) -> ok when
    Module :: atom(),
    Function :: atom(),
    FunctionArgs :: [term()].
execute_command(Module, Function, [DispatcherPid, Uid | CommandArgs] = FunctionArgs) ->
    try
        apply(Module, Function, FunctionArgs)
    catch
        Type:Reason ->
            nls_server:response_content(commands, [{nls, invalid_argument}, CommandArgs, <<"\n\n">>, {nls, list_to_atom(atom_to_list(Module) ++ "_help")}], player_fsm:get_lang(Uid), DispatcherPid),
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
            throw(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parse the raw command from user input to original command module name.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_raw_command(RawCommand) -> command() | atom() when
    RawCommand :: atom().
parse_raw_command('5') -> look;
parse_raw_command(l) -> look;

parse_raw_command(Other) -> Other.