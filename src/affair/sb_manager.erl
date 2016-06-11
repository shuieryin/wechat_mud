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
    register/2
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

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
    % TODO execute command to get registered username & passwords from starbound.config, convert and put it to NpcContext
    io:format("TODO execute command to get registered username & passwords from starbound.config, convert and put it to NpcContext~n"),
    NpcContext.

%%--------------------------------------------------------------------
%% @doc
%% Register SB account and password.
%%
%% @end
%%--------------------------------------------------------------------
-spec register(NpcState, CommandContext) -> {UpdatedNpcState, UpdatedCommandContext} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext.
register(NpcState, #command_context{
    command_args = #affair_context{
        from_player = #player_profile{
            id = PlayerId
        }
    } = AffairContext
} = CommandContext) ->
    ResponseMessage =
        case elib:connect_node('starbound_support@starbound.local') of
            true ->
                case gen_server:call({global, starbound_common_server}, {user, PlayerId}) of
                    undefined ->
                        PasswordSrc = uuid:uuid_to_string(uuid:get_v4()),
                        RawPassword = re:split(PasswordSrc, "-"), % already converted to binary
                        NewPassword = lists:nth(random:uniform(3) + 1, RawPassword),
                        RestartMessage =
                            case gen_server:call({global, starbound_common_server}, {add_user, PlayerId, NewPassword}) of
                                done ->
                                    {nls, sb_restarted};
                                pending ->
                                    {nls, sb_restart_pending}
                            end,
                        [{nls, sb_registered_success}, <<"\n">>, {nls, sb_account_password, [PlayerId, NewPassword]}, <<"\n">>, {nls, sb_check_password}, <<"\n\n">>, RestartMessage, <<"\n">>];
                    {Password, IsPendingRestart} ->
                        RestartMessage =
                            case IsPendingRestart of
                                true ->
                                    {nls, sb_restart_pending};
                                false ->
                                    <<"">>
                            end,
                        [{nls, sb_account_already_registered}, <<"\n">>, {nls, sb_account_password, [PlayerId, Password]}, <<"\n">>, {nls, sb_check_password}, <<"\n\n">>, RestartMessage, <<"\n">>]
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
        }
    }.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================