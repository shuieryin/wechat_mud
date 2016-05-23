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

-define(SB_REGISTERED_PLAYERS_INFO, sb_registered_players_info).

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
    NpcContext#{?SB_REGISTERED_PLAYERS_INFO => #{}}.

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
register(#npc_state{
    npc_context = #{
        ?SB_REGISTERED_PLAYERS_INFO := SBRegisteredPlayerInfo
    } = NpcContext
} = NpcState, #command_context{
    command_args = #affair_context{
        from_player = #player_profile{
            id = PlayerId
        }
    } = AffairContext
} = CommandContext) ->
    % TODO implement logic to register SB ID.
    {UpdatedSBRegisteredPlayerInfo, ResponseMessage} =
        case maps:get(PlayerId, SBRegisteredPlayerInfo, undefined) of
            undefined ->
                NewPassword = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                % TODO execute command to write PlayerId as username and the new generated password into starbound.config

                PasswordMessage = [{nls, sb_registered_success}, <<"\n">>, {nls, sb_account_password, [PlayerId, NewPassword]}, <<"\n">>, {nls, sb_check_password}, <<"\n">>],
                {SBRegisteredPlayerInfo#{PlayerId => NewPassword}, PasswordMessage};
            Password ->
                PasswordMessage = [{nls, sb_account_already_registered}, <<"\n">>, {nls, sb_account_password, [PlayerId, Password]}, <<"\n">>, {nls, sb_check_password}, <<"\n">>],
                {SBRegisteredPlayerInfo, PasswordMessage}
        end,
    {
        NpcState#npc_state{
            npc_context = NpcContext#{
                ?SB_REGISTERED_PLAYERS_INFO := UpdatedSBRegisteredPlayerInfo
            }
        },
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = ResponseMessage
            }
        }
    }.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================