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
    register/1
]).

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
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
register(#affair_context{
    from_player = #player_profile{
        uid = Uid,
        id = PlayerId
    },
    dispatcher_pid = DispatcherPid,
    to_target = #npc_state{
        npc_context = #{
            ?SB_REGISTERED_PLAYERS_INFO := SBRegisteredPlayerInfo
        } = NpcContext
    } = NpcState
}) ->
    % TODO implement logic to register SB ID.
    UpdatedSBRegisteredPlayerInfo =
        case maps:get(PlayerId, SBRegisteredPlayerInfo, undefined) of
            undefined ->
                NewPassword = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
                % TODO execute command to write PlayerId as username and the new generated password into starbound.config

                PasswordMessage = [{nls, sb_registered_success}, <<"\n">>, {nls, sb_account_password, [PlayerId, NewPassword]}, <<"\n">>, {nls, sb_check_password}, <<"\n">>],
                player_fsm:response_content(Uid, PasswordMessage, DispatcherPid),
                SBRegisteredPlayerInfo#{PlayerId => NewPassword};
            Password ->
                PasswordMessage = [{nls, sb_account_already_registered}, <<"\n">>, {nls, sb_account_password, [PlayerId, Password]}, <<"\n">>, {nls, sb_check_password}, <<"\n">>],
                player_fsm:response_content(Uid, PasswordMessage, DispatcherPid),
                SBRegisteredPlayerInfo
        end,
    NpcState#npc_state{
        npc_context = NpcContext#{
            ?SB_REGISTERED_PLAYERS_INFO := UpdatedSBRegisteredPlayerInfo
        }
    }.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================