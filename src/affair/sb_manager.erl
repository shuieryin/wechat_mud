%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% This module is for managing of starbound server.
%%%
%%% @end
%%% Created : 21. May 2016 10:39 PM
%%%-------------------------------------------------------------------
-module(sb_manager).
-author("shuieryin").

%% API
-export([
    init/2,
    register/3,
    status/3,
    ip/3,
    start/3,
    apply_admin/3,
    feedback/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

-define(SB_NODE, 'starbound_support@powerteam.mynetgear.com').
-define(SB_GEN_SERVER, starbound_common_server).

-record(player_info, {
    player_name :: binary(),
    ip_addr :: inet:ip4_address(),
    last_login_time :: erlang:timestamp(),
    agree_restart = false :: boolean()
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
    NpcContext.

%%--------------------------------------------------------------------
%% @doc
%% Register SB account and password.
%%
%% @end
%%--------------------------------------------------------------------
-spec register(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
register(NpcState, #command_context{
    command_args = #affair_context{
        from_player = #player_profile{
            id = PlayerId
        }
    } = AffairContext
} = CommandContext, StateName) ->
    ResponseMessage =
        case elib:connect_node(?SB_NODE) of
            true ->
                case gen_server:call({global, ?SB_GEN_SERVER}, {user, PlayerId}) of
                    undefined ->
                        PasswordSrc = uuid:uuid_to_string(uuid:get_v4()),
                        RawPassword = re:split(PasswordSrc, "-"), % already converted to binary
                        NewPassword = lists:nth(rand:uniform(3) + 1, RawPassword),
                        RestartMessage =
                            case gen_server:call({global, ?SB_GEN_SERVER}, {add_user, PlayerId, NewPassword}) of
                                done ->
                                    {nls, sb_restarted};
                                pending ->
                                    {nls, sb_restart_pending}
                            end,
                        [
                            {nls, sb_registered_success}, <<"\n">>,
                            {nls, sb_account_password, [PlayerId, NewPassword]}, <<"\n">>,
                            {nls, sb_check_password}, <<"\n\n">>,
                            RestartMessage
                        ];
                    {Password, IsPendingRestart, BanReason} ->
                        BanMessages =
                            case BanReason of
                                undefined ->
                                    <<"">>;
                                _Banned ->
                                    [<<"\n">>, {nls, sb_account_inactivated}, {nls, BanReason}]
                            end,

                        RestartMessage =
                            case IsPendingRestart of
                                true ->
                                    [<<"\n\n">>, {nls, sb_restart_pending}];
                                false ->
                                    <<"">>
                            end,

                        lists:flatten([
                            {nls, sb_account_already_registered}, <<"\n">>,
                            {nls, sb_account_password, [PlayerId, Password]}, <<"\n">>,
                            {nls, sb_check_password},
                            BanMessages,
                            RestartMessage
                        ])
                end;
            _NoConnection ->
                [{nls, sb_server_offline}, <<"\n">>]
        end,
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = ResponseMessage
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Check SB server status.
%%
%% @end
%%--------------------------------------------------------------------
-spec status(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
status(NpcState, #command_context{
    command_args = AffairContext
} = CommandContext, StateName) ->
    ServerDownMessage = [{nls, sb_server_offline}, <<"\n">>],

    ResponseMessage =
        case elib:connect_node(?SB_NODE) of
            true ->
                #{
                    is_sb_server_up := IsSbServerUp,
                    online_users := OnlineUsers,
                    memory_usage := MemoryUsage,
                    temperature := Temperature,
                    cpu_usage := CpuUsage,
                    admin_player := AdminPlayer
                } = gen_server:call({global, ?SB_GEN_SERVER}, server_status),
                case IsSbServerUp of
                    true ->
                        OnlinePlayerNames = maps:fold(
                            fun(_Username, #player_info{player_name = PlayerName}, AccPlayerNames) ->
                                [<<"[">>, re:replace(PlayerName, <<"\\^[\\w\\d#]+\\;">>, <<>>, [global, {return, binary}]), <<"]\n">> | AccPlayerNames]
                            end, [], OnlineUsers),
                        PlayerStatus =
                            case OnlinePlayerNames of
                                [] ->
                                    [{nls, sb_no_one_online}, <<"\n">>];
                                _Else ->
                                    [{nls, sb_online_players}, <<"\n">>, OnlinePlayerNames]
                            end,

                        CurrentGm = case AdminPlayer of
                                        undefined ->
                                            <<"[]">>;
                                        {PlayerId, _ExpireTime} ->
                                            <<"[", PlayerId/binary, "]">>
                                    end,

                        [
                            PlayerStatus,
                            [
                                {nls, sb_memory_usage}, MemoryUsage,
                                <<"\n">>,
                                {nls, sb_cpuusage}, CpuUsage,
                                <<"\n">>,
                                {nls, sb_temperature}, Temperature,
                                <<"\n">>,
                                {nls, current_gm}, CurrentGm
                            ]
                        ];
                    false ->
                        ServerDownMessage
                end;
            _NoConnection ->
                ServerDownMessage
        end,
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = lists:flatten(ResponseMessage)
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Get SB server ip.
%%
%% @end
%%--------------------------------------------------------------------
-spec ip(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
ip(NpcState, #command_context{
    command_args = AffairContext
} = CommandContext, StateName) ->
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [list_to_binary(os:cmd("curl -s ifconfig.co"))]
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Safe start starbound server
%%
%% @end
%%--------------------------------------------------------------------
-spec start(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
start(NpcState, #command_context{
    command_args = AffairContext
} = CommandContext, StateName) ->
    ServerDownMessage = [{nls, sb_server_offline}, <<"\n">>],

    ResponseMessage =
        case elib:connect_node(?SB_NODE) of
            true ->
                case gen_server:call({global, ?SB_GEN_SERVER}, safe_start_cmd) of
                    <<"server already started">> ->
                        [{nls, server_already_started}, <<"\n">>];
                    _Success ->
                        [{nls, server_started}, <<"\n">>]
                end;
            _NoConnection ->
                ServerDownMessage
        end,
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = lists:flatten(ResponseMessage)
            }
        }, StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Apply admin right.
%%
%% @end
%%--------------------------------------------------------------------
-spec apply_admin(NpcState, CommandContext, StateName) -> {UpdatedNpcState, UpdatedCommandContext, UpdatedStateName} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext,
    StateName :: gen_statem:state_name(),
    UpdatedStateName :: StateName.
apply_admin(NpcState, #command_context{
    command_args = #affair_context{
        from_player = #player_profile{
            id = PlayerId
        }
    } = AffairContext
} = CommandContext, StateName) ->
    ServerDownMessage = [{nls, sb_server_offline}, <<"\n">>],
    ResponseMessage =
        case elib:connect_node(?SB_NODE) of
            true ->
                Status = gen_server:call({global, ?SB_GEN_SERVER}, {make_player_admin, PlayerId}),
                [{nls, Status}, <<"\n">>];
            _NoConnection ->
                ServerDownMessage
        end,
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = lists:flatten(ResponseMessage)
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
feedback(State, _CommandContext, StateName) ->
    {State, StateName}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================