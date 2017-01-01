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

-define(BILIBILI_NODE, 'bilibili@affair.local').
-define(BILIBILI_GEN_SERVER, bilibili_common_server).

-record(affair_action_bilibili_manager, {
    action_id :: atom(),
    action_desc :: nls_server:nls_object()
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
        affair_action_name := AffairActionName
    } = NpcContext
} = NpcState, #command_context{
    command_args = AffairContext,
    from = #simple_player{
        uid = Uid
    }
} = CommandContext, StateName) ->
    {
        NpcState,
        CommandContext#command_context{
            command_args = AffairContext#affair_context{
                response_message = [{nls, AffairActionName}, <<"\n">>] ++ menu_display(NpcContext, Uid),
                affair_data = NpcContext
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
        uid = Uid,
        scene = SceneName
    },
    current_affair = {_AffairName, NpcContext}
} = PlayerState, DispatcherPid, RawInput) ->
    case RawInput of
        <<"0">> ->
            scene_fsm:show_scene(SceneName, Uid, DispatcherPid),
            {next_state, non_battle, PlayerState#player_state{
                current_affair = {undefined, undefined}
            }};
        <<"1">> ->
            % TODO connect bilibili upload process
            player_statem:do_response_content(PlayerState, [<<"Working on it.\n">>] ++ menu_display(NpcContext, Uid), DispatcherPid),
            keep_state_and_data;
        InvalidCommand ->
            player_statem:do_response_content(PlayerState, [{nls, invalid_command}, InvalidCommand, <<"\n">>] ++ menu_display(NpcContext, Uid), DispatcherPid),
            keep_state_and_data
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Return menu display content.
%%
%% @end
%%--------------------------------------------------------------------
-spec menu_display(NpcContext, PlayerUid) -> ReturnContent when
    NpcContext :: map(),
    PlayerUid :: player_statem:uid(),
    ReturnContent :: [nls_server:nls_object()].
menu_display(#{
    affair_action_data := AffairData,
    wizard_uids := WizardUis
}, PlayerUid) ->
    io:format("AffairData:~p~n", [AffairData]),
    InitReturn = {0, []},
    {_MenuItemCount, MenuList}
        = case maps:is_key(PlayerUid, WizardUis) of
              false ->
                  InitReturn;
              true ->
                  maps:fold(
                      fun(_CurActionId, AffairValue, {AccMenuItemCount, AccMenuList}) ->
                          io:format("AffairValue:~p~n", [AffairValue]),
                          #affair_action_bilibili_manager{
                              action_desc = ActionDescNls
                          } = AffairValue,
                          UpdatedAccMenuItemCount = AccMenuItemCount + 1,
                          UpdatedAccMenuItemCountBin = integer_to_binary(UpdatedAccMenuItemCount),
                          {UpdatedAccMenuItemCount, [<<"\n", UpdatedAccMenuItemCountBin/binary, ": ">>, ActionDescNls | AccMenuList]}
                      end, InitReturn, AffairData
                  )
          end,

    [{nls, affair_menu}] ++ MenuList ++ [<<"\n0: ">>, {nls, affiar_menu_exit}].