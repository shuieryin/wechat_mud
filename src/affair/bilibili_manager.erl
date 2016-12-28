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
    help/2
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

-define(BILIBILI_NODE, 'bilibili@starbound.local').
-define(BILIBILI_GEN_SERVER, bilibili_common_server).

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
%% Show help text. May make it a common method in future.
%%
%% @end
%%--------------------------------------------------------------------
-spec help(NpcState, CommandContext) -> {UpdatedNpcState, UpdatedCommandContext} when
    NpcState :: #npc_state{},
    CommandContext :: #command_context{},
    UpdatedNpcState :: NpcState,
    UpdatedCommandContext :: CommandContext.
help(#npc_state{
    self = #npc_profile{
        npc_id = NpcId
    }
} = NpcState, #command_context{
    command_args = AffairContext
} = CommandContext) ->
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [{nls, binary_to_atom(<<NpcId/binary, "_help">>, utf8)}, <<"\n">>]
            }
        }
    }.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================