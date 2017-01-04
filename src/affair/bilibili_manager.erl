%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% This module is for managing of bilibili videos.
%%%
%%% @end
%%% Created : 21. May 2016 10:39 PM
%%%-------------------------------------------------------------------
-module(bilibili_manager).
-author("shuieryin").

%% API
-export([
    init/2,
    manage/3,
    help/3,
    feedback/3,
    handle_affair_input/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

-define(BILIBILI_NODE, <<"http://192.168.1.123:4567/bilibili_manager">>).
-define(BILIBILI_ACCOUNT_PROFILES, bilibili_account_profiles).
-define(INIT_MENU_DATA, #{0 => exit_menu}).
-define(SERVER_DOMAIN, server_domain).

-type event_type() :: input_credentials | init_browser_session | close_browser_session | input_captcha.
-type menu_name() :: atom().
-type menu_data() :: #{integer() => menu_name()}.

-record(affair_action_bilibili_manager, {
    action_id :: atom(),
    action_desc :: nls_server:nls_object()
}).

-record(account_profile, {
    account_name :: binary(),
    password :: binary()
}).

-record(bilibili_manager_context, {
    affair_action_data :: map(),
    wizard_uids :: #{player_statem:uid() => term()},
    affair_action_name :: nls_server:key(),
    account_profiles :: #{atom() => #account_profile{}},
    menu_desc_nls :: nls_server:nls_object(),
    menu_data = #{} :: menu_data(),
    bilibili_state_name = menu :: menu | input_username_password | input_captcha,
    server_domain :: binary(),
    captcha_url :: binary() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialization method has to be implemented for preparing NpcContext.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(NpcProfile, NpcContext) -> UpdatedNpcContext when
    NpcProfile :: #npc_profile{},
    NpcContext :: map(),
    UpdatedNpcContext :: NpcContext.
init(_NpcProfile, NpcContext) ->
    ModuleNameBin = list_to_binary(?MODULE_STRING),
    AffairActionName = binary_to_atom(<<"affair_action_", ModuleNameBin/binary>>, utf8),

    #{
        AffairActionName := AffairActionData,
        wizard_uids := WizardUis
    } = common_server:runtime_datas([AffairActionName, wizard_uids]),

    BilibiliProfilesMap =
        case redis_client_server:get(?BILIBILI_ACCOUNT_PROFILES) of
            undefined ->
                ProfilesMap = #{},
                true = redis_client_server:set(?BILIBILI_ACCOUNT_PROFILES, ProfilesMap, true),
                ProfilesMap;
            ExistingProfilesMap ->
                ExistingProfilesMap
        end,

    NpcContext#{
        bilibili_manager_context => #bilibili_manager_context{
            affair_action_data = AffairActionData,
            wizard_uids = WizardUis,
            affair_action_name = AffairActionName,
            account_profiles = BilibiliProfilesMap,
            server_domain = redis_client_server:get(?SERVER_DOMAIN)
        }
    }.

%%--------------------------------------------------------------------
%% @doc
%% Bilibili management
%%
%% @end
%%--------------------------------------------------------------------
-spec manage(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
manage(#npc_state{
    npc_context = #{
        bilibili_manager_context := #bilibili_manager_context{
            affair_action_name = AffairActionName
        } = BilibiliManagerContext
    } = NpcContext
} = NpcState, #command_context{
    command_args = AffairContext,
    from = #simple_player{
        uid = Uid
    }
} = CommandContext, StateName) ->
    {MenuDescNls, MenuData} = init_menu(BilibiliManagerContext, Uid),
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [{nls, AffairActionName}, <<"\n">>] ++ MenuDescNls,
                affair_data = NpcContext#{
                    bilibili_manager_context := BilibiliManagerContext#bilibili_manager_context{
                        menu_desc_nls = MenuDescNls,
                        menu_data = MenuData
                    }
                }
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Show help text. May make it a common method in future.
%%
%% @end
%%--------------------------------------------------------------------
-spec help(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
help(#npc_state{
    self = #npc_profile{
        npc_id = NpcId
    }
} = NpcState, #command_context{
    command_args = AffairContext
} = CommandContext, StateName) ->
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [{nls, binary_to_atom(<<NpcId/binary, "_help">>, utf8)}, <<"\n">>]
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Handle feedback for from target.
%%
%% @end
%%--------------------------------------------------------------------
-spec feedback(State, CommandContext, StateName) -> {UpdatedState, UpdatedStateName} when
    State :: #player_state{},
    UpdatedState :: State,
    CommandContext :: #command_context{},
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
feedback(State, #command_context{
    command_args = AffairContext
}, _StateName) ->
    {CurrentAffairName, CurrentAffairData}
        = case AffairContext of
              #affair_context{
                  affair_name = AffairName,
                  affair_data = AffairData
              } ->
                  case AffairName of
                      <<"manage">> ->
                          {?MODULE, AffairData};
                      _Other ->
                          {undefined, undefined}
                  end;
              _Other ->
                  {undefined, undefined}
          end,

    {
        State#player_state{
            current_affair = {CurrentAffairName, CurrentAffairData}
        },
        affair_menu
    }.

%%--------------------------------------------------------------------
%% @doc
%% Handle affair state machine inputs from player.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_affair_input(PlayerState, DispatcherPid, RawInput) -> StateFunctionResult when
    PlayerState :: #player_state{},
    DispatcherPid :: pid(),
    UpdatePlayerState :: PlayerState,

    Action :: gen_statem:action(),
    RawInput :: term(),

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, UpdatePlayerState, Data}.
handle_affair_input(#player_state{
    self = #player_profile{
        uid = PlayerUid,
        scene = SceneName
    },
    current_affair = {AffairName, #{
        bilibili_manager_context := #bilibili_manager_context{
            menu_desc_nls = MenuDescNls,
            account_profiles = AccountProfilesMap,
            bilibili_state_name = BilibiliStateName,
            captcha_url = CaptchaUrl,
            menu_data = MenuData
        } = BilibiliManagerContext
    } = AffairData}
} = PlayerState, DispatcherPid, RawInput) ->
    case BilibiliStateName of
        menu ->
            case maps:get(binary_to_integer(RawInput), MenuData, undefined) of
                exit_menu ->
                    request(PlayerUid, {close_browser_session, []}),
                    UpdatedPlayerState = PlayerState#player_state{
                        current_affair = {undefined, undefined}
                    },

                    scene_fsm:show_scene(SceneName, PlayerUid, DispatcherPid),
                    {next_state, non_battle, UpdatedPlayerState};
                update_bilibili_profile ->
                    player_statem:do_response_content(PlayerState, [{nls, input_username_password_format}], DispatcherPid),
                    {next_state, affair_menu, PlayerState#player_state{
                        current_affair = {AffairName, AffairData#{
                            bilibili_manager_context := BilibiliManagerContext#bilibili_manager_context{
                                bilibili_state_name = input_username_password
                            }
                        }}
                    }};
                login_bilibili ->
                    login_bilibili(PlayerState, DispatcherPid);
                _InvalidCommand ->
                    player_statem:do_response_content(PlayerState, [{nls, invalid_command}, RawInput, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                    keep_state_and_data
            end;
        input_username_password ->
            case re:run(RawInput, <<"99\\s+(\\S+)\\s+(\\S+)">>, [{capture, all, binary}]) of
                nomatch ->
                    player_statem:do_response_content(PlayerState, [{nls, input_username_password_format}], DispatcherPid),
                    keep_state_and_data;
                {match, [_TrimedInput, Username, Password]} ->
                    UpdatedAccountProfilesMap = AccountProfilesMap#{
                        PlayerUid => #account_profile{
                            account_name = Username,
                            password = Password
                        }
                    },

                    UpdatedPlayerState =
                        PlayerState#player_state{
                            current_affair = {AffairName, AffairData#{
                                bilibili_manager_context := BilibiliManagerContext#bilibili_manager_context{
                                    bilibili_state_name = menu,
                                    account_profiles = UpdatedAccountProfilesMap
                                }
                            }}
                        },

                    true = redis_client_server:set(?BILIBILI_ACCOUNT_PROFILES, UpdatedAccountProfilesMap, true),
                    player_statem:do_response_content(PlayerState, [{nls, bilibili_profile_updated}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                    {next_state, affair_menu, UpdatedPlayerState}
            end;
        input_captcha ->
            UpdatedPlayerState =
                PlayerState#player_state{
                    current_affair = {AffairName, AffairData#{
                        bilibili_manager_context := BilibiliManagerContext#bilibili_manager_context{
                            bilibili_state_name = menu
                        }
                    }}
                },

            case re:run(RawInput, <<"98\\s+(\\S+)">>, [{capture, all, binary}]) of
                nomatch ->
                    player_statem:do_response_content(PlayerState, [{nls, please_input_captcha}, <<"\n">>, CaptchaUrl], DispatcherPid),
                    {next_state, affair_menu, UpdatedPlayerState};
                {match, [_TrimedInput, CaptchaInputFromUser]} ->
                    error_logger:info_msg("RawCaptchaInputFromUser:~p~n", [CaptchaInputFromUser]),
                    ServerResponse = request(PlayerUid, {input_captcha, [{<<"input_captcha">>, CaptchaInputFromUser}]}),
                    error_logger:info_msg("ServerResponse:~p~n", [ServerResponse]),
                    case ServerResponse of
                        undefined ->
                            player_statem:do_response_content(PlayerState, [{nls, bilibili_manager_offline}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                            keep_state_and_data;
                        {struct, JsonObjectList} ->
                            {<<"status">>, InputCaptchaStatus} = lists:keyfind(<<"status">>, 1, JsonObjectList),
                            case InputCaptchaStatus of
                                true ->
                                    player_statem:do_response_content(PlayerState, [{nls, login_sucess}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                                    {next_state, affair_menu, UpdatedPlayerState};
                                _LoginFailed ->
                                    login_bilibili(UpdatedPlayerState, DispatcherPid)
                            end
                    end
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check whether player uid is registered wizard for bilibili management.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_wizard(Uid, #bilibili_manager_context{}) -> boolean() when
    Uid :: player_statem:uid().
is_wizard(PlayerUid, #bilibili_manager_context{
    wizard_uids = WizardUis
}) ->
    maps:is_key(PlayerUid, WizardUis).

%%--------------------------------------------------------------------
%% @doc
%% Return menu display content.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_menu(BilibiliManagerContext, PlayerUid) -> {MenuNls, MenuData} when
    BilibiliManagerContext :: #bilibili_manager_context{},
    PlayerUid :: player_statem:uid(),
    MenuNls :: [nls_server:nls_object()],
    MenuData :: menu_data().
init_menu(#bilibili_manager_context{
    affair_action_data = AffairData
} = BilibiliManagerContext, PlayerUid) ->
    InitReturn = {0, [], ?INIT_MENU_DATA},
    {_MenuItemCount, MenuNlsObjectList, UpdatedMenuData}
        = case is_wizard(PlayerUid, BilibiliManagerContext) of
              false ->
                  InitReturn;
              true ->
                  spawn(
                      fun() ->
                          request(PlayerUid, {init_browser_session, []})
                      end
                  ),
                  maps:fold(
                      fun(CurActionId, AffairValue, {AccMenuItemCount, AccMenuList, AccMenuData}) ->
                          #affair_action_bilibili_manager{
                              action_desc = ActionDescNls
                          } = AffairValue,
                          UpdatedAccMenuItemCount = AccMenuItemCount + 1,
                          UpdatedAccMenuItemCountBin = integer_to_binary(UpdatedAccMenuItemCount),
                          {
                              UpdatedAccMenuItemCount, [
                              <<"\n", UpdatedAccMenuItemCountBin/binary, ": ">>, ActionDescNls | AccMenuList],
                              AccMenuData#{
                                  UpdatedAccMenuItemCount => CurActionId
                              }
                          }
                      end, InitReturn, AffairData
                  )
          end,

    FinalMenuNls = [{nls, affair_menu}] ++ MenuNlsObjectList ++ [<<"\n0: ">>, {nls, affiar_menu_exit}],
    {FinalMenuNls, UpdatedMenuData}.

%%--------------------------------------------------------------------
%% @doc
%% Return menu display content.
%%
%% @end
%%--------------------------------------------------------------------
-spec request(Uid, {event_type(), EventParams}) -> term() when
    Uid :: player_statem:uid(),
    EventParams :: [{Key :: binary(), Value :: binary()}].
request(Uid, {Event, EventParams}) ->
    UidBin = atom_to_binary(Uid, utf8),
    EventBin = atom_to_binary(Event, utf8),
    EventParamsBin = lists:foldl(
        fun({Key, Value}, AccEventParamsBin) ->
            <<Key/binary, "=", Value/binary, "&", AccEventParamsBin/binary>>
        end, <<>>, EventParams
    ),

    RequestParams = {
        % URI
        binary_to_list(<<
            ?BILIBILI_NODE/binary,
            "?uid=", UidBin/binary,
            "&event=", EventBin/binary,
            "&", EventParamsBin/binary
        >>),

        % Headers
        [
            {
                "Content-Type", "application/x-www-form-urlencoded",
                "timestamp", integer_to_list(elib:timestamp())
            }
        ],

        % Content type
        "raw",

        %Body
        <<>>
    },

    error_logger:info_msg("Sending request:~p~n", [RequestParams]),

    Response = httpc:request(
        % Method
        post,

        % Request
        RequestParams,

        % Http options
        [{ssl, [{verify, 0}]}],

        % Options
        []
    ),

    case Response of
        {ok, {{_HttpVersion, _HttpStatusCode, _OK}, _ResponseHeaders, BodyStr}} ->
            error_logger:info_msg("HttpResponse:~p~n", [Response]),
            case BodyStr of
                [] ->
                    undefined;
                _Json ->
                    mochijson2:decode(BodyStr)
            end;
        Error ->
            error_logger:error_msg("~p", [Error]),
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Do login bilibili on browser session.
%%
%% @end
%%--------------------------------------------------------------------
-spec login_bilibili(PlayerState, DispatcherPid :: pid()) -> StateFunctionResult when
    PlayerState :: #player_state{},
    UpdatedPlayerState :: PlayerState,
    Action :: gen_statem:action(),

    StateFunctionResult :: gen_statem:event_handler_result(PlayerState) |
    {keep_state_and_data, Action} |
    {next_state, gen_statem:state_name(), UpdatedPlayerState}.
login_bilibili(#player_state{
    self = #player_profile{
        uid = PlayerUid
    },
    current_affair = {AffairName, #{
        bilibili_manager_context := #bilibili_manager_context{
            menu_desc_nls = MenuDescNls,
            account_profiles = AccountProfilesMap,
            server_domain = ServerDomain
        } = BilibiliManagerContext
    } = AffairData}
} = PlayerState, DispatcherPid) ->
    case maps:get(PlayerUid, AccountProfilesMap, undefined) of
        undefined ->
            player_statem:do_response_content(PlayerState, [{nls, please_update_bilibili_profile}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
            keep_state_and_data;
        #account_profile{
            account_name = Username,
            password = Password
        } ->
            RawResponse = request(PlayerUid, {
                input_credentials,
                [
                    {<<"username">>, Username},
                    {<<"password">>, Password}
                ]
            }),

            case RawResponse of
                undefined ->
                    player_statem:do_response_content(PlayerState, [{nls, bilibili_manager_offline}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                    keep_state_and_data;
                {struct, JsonObjectList} ->
                    {<<"is_logged_on">>, IsLoggedOn} = lists:keyfind(<<"is_logged_on">>, 1, JsonObjectList),
                    case IsLoggedOn of
                        true ->
                            player_statem:do_response_content(PlayerState, [{nls, already_login}, <<"\n">>] ++ MenuDescNls, DispatcherPid),
                            keep_state_and_data;
                        _LoggedOn ->
                            {<<"captcha_image_bytes">>, CaptchaImageBytes} = lists:keyfind(<<"captcha_image_bytes">>, 1, JsonObjectList),
                            ImagePath = filename:join(["assets", "captchaImg.png"]),
                            CaptchaImageFilePath = filename:join([code:priv_dir(wechat_mud), ImagePath]),
                            {ok, CaptchaImageFile} = file:open(CaptchaImageFilePath, [write, binary]),
                            ok = file:write(CaptchaImageFile, base64:decode(CaptchaImageBytes)),
                            file:close(CaptchaImageFile),
                            error_logger:info_msg("Saved captcha image to: ~p~n", [CaptchaImageFilePath]),

                            ImagePathBin = list_to_binary(ImagePath),
                            ImageUrl = <<"http://", ServerDomain/binary, "/", ImagePathBin/binary>>,

                            UpdatedPlayerState = PlayerState#player_state{
                                current_affair = {AffairName, AffairData#{
                                    bilibili_manager_context := BilibiliManagerContext#bilibili_manager_context{
                                        bilibili_state_name = input_captcha,
                                        captcha_url = ImageUrl
                                    }
                                }}
                            },

                            player_statem:do_response_content(PlayerState, [{nls, please_input_captcha}, <<"\n">>, ImageUrl], DispatcherPid),
                            {next_state, affair_menu, UpdatedPlayerState}
                    end
            end
    end.