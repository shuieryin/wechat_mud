%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Generic direction module
%%%
%%% @end
%%% Created : 20. Sep 2015 4:08 PM
%%%-------------------------------------------------------------------
-module(direction).
-author("shuieryin").

%% API
-export([
    exec/3,
    parse_direction/1,
    go_direction/3
]).

-type directions() :: east | south | west | north | northeast | southeast | southwest | northwest.
-export_type([directions/0]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Execute user input direction, the "Direction" parameter has been
%% validated ahead in "command_dispatcher" module.
%%
%% This function returns "ok" immeidately and the scene info will
%% be responsed to user from player_fsm by sending responses to
%% DispatcherPid process.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, Direction) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    Direction :: directions().
exec(DispatcherPid, Uid, Direction) ->
    CommandContext = #command_context{
        command_func = go_direction,
        command_args = Direction,
        dispatcher_pid = DispatcherPid
    },
    cm:execute_command(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Go to direction.
%%
%% This function does the followings:
%%
%% 1. Checks if current scene is linked to the target scene, if so go to
%% step 2, otherwise remind user the direction is invalid.
%%
%% 2. Leave the current scene, enter the target scene, and display the
%% target scene info to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec go_direction(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
go_direction(#command_context{command_args = Direction, dispatcher_pid = DispatcherPid}, #player_state{self = #player_profile{scene = CurSceneName, uid = Uid} = PlayerProfile} = State, StateName) ->
    {TargetSceneName, UpdatedState} =
        case scene_fsm:go_direction(CurSceneName, Uid, Direction) of
            undefined ->
                {CurSceneName, player_fsm:do_response_content(State, [{nls, invalid_exit}], DispatcherPid)};
            NewSceneName ->
                ok = scene_fsm:enter(NewSceneName, DispatcherPid, player_fsm:simple_player(PlayerProfile), CurSceneName),
                {NewSceneName, State}
        end,
    {ok, StateName, UpdatedState#player_state{self = PlayerProfile#player_profile{scene = TargetSceneName}}}.

%%--------------------------------------------------------------------
%% @doc
%% Parse the direction atom from user input. This function has to be
%% called from command_dispactcher module because of the direction input
%% does not has a fixed command prefix and so it has to be pre-determined
%% at the very last when no other commands matched.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_direction(RawDirectionInput) -> directions() | undefined when
    RawDirectionInput :: binary(). % generic binary
parse_direction(<<"6">>) -> east;
parse_direction(<<"8">>) -> south;
parse_direction(<<"4">>) -> west;
parse_direction(<<"2">>) -> north;
parse_direction(<<"3">>) -> northeast;
parse_direction(<<"9">>) -> southeast;
parse_direction(<<"7">>) -> southwest;
parse_direction(<<"1">>) -> northwest;

parse_direction(<<"e">>) -> east;
parse_direction(<<"s">>) -> south;
parse_direction(<<"w">>) -> west;
parse_direction(<<"n">>) -> north;
parse_direction(<<"ne">>) -> northeast;
parse_direction(<<"se">>) -> southeast;
parse_direction(<<"sw">>) -> southwest;
parse_direction(<<"nw">>) -> northwest;

parse_direction(<<"east">>) -> east;
parse_direction(<<"south">>) -> south;
parse_direction(<<"west">>) -> west;
parse_direction(<<"north">>) -> north;
parse_direction(<<"northeast">>) -> northeast;
parse_direction(<<"southeast">>) -> southeast;
parse_direction(<<"southwest">>) -> southwest;
parse_direction(<<"northwest">>) -> northwest;

parse_direction(_) -> undefined.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================