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
    handle_affair_input/2
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/ask.hrl").

-define(BILIBILI_NODE, 'bilibili@affair.local').
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
    ModuleNameBin = list_to_binary(?MODULE_STRING),
    AffairActionName = binary_to_atom(<<"affair_action_", ModuleNameBin/binary>>, utf8),

    #{
        AffairActionName := AffairActionData,
        wizard_uids := WizardUis
    } = common_server:runtime_datas([AffairActionName, wizard_uids]),

    NpcContext#{
        affair_action_data => AffairActionData,
        wizard_uids => WizardUis,
        affair_action_name => AffairActionName
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
        affair_action_data := AffairActionData,
        wizard_uids := WizardUis,
        affair_action_name := AffairActionName
    }
} = NpcState, #command_context{
    command_args = AffairContext,
    from = #simple_player{
        uid = PlayerUid
    }
} = CommandContext, _StateName) ->
    error_logger:info_msg("AffairActionData:~p~nWizardUis:~p~nPlayerUid:~p~nAffairActionName:~p~n", [AffairActionData, WizardUis, PlayerUid, AffairActionName]),
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [{nls, AffairActionName}, <<"\n">>, {nls, affair_menu}, <<"\n0: ">>, {nls, affiar_menu_exit}]
            }
        }, affair_menu
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
    command_func = CurrentCommandFuncName
}, StateName) ->
    CurrentAffairName =
        case CurrentCommandFuncName of
            manage ->
                ?MODULE;
            _Other ->
                undefined
        end,
    io:format("execute_command CurrentAffairName:~p~n", [CurrentAffairName]),
    {
        State#player_state{
            current_affair_name = CurrentAffairName
        },
        StateName
    }.

%%--------------------------------------------------------------------
%% @doc
%% Handle affair state machine inputs from player.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_affair_input(PlayerState, RawInput) -> StateFunctionResult when
    PlayerState :: #player_state{},
    UpdatePlayerState :: PlayerState,

    Action :: gen_statem:action(),
    RawInput :: term(),

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, UpdatePlayerState, Data}.
handle_affair_input(PlayerState, RawInput) ->
    error_logger:info_msg("RawInput:~p~n", [RawInput]),
    {next_state, affair_menu, PlayerState}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================