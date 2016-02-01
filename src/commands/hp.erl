%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Display player hp status.
%%%
%%% @end
%%% Created : 16. Jan 2016 7:54 PM
%%%-------------------------------------------------------------------
-module(hp).
-author("shuieryin").

%% API
-export([
    exec/2,
    show_hp/3
]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Display player hp status.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid().
exec(DispatcherPid, Uid) ->
    CommandContext = #command_context{
        command_func = show_hp,
        dispatcher_pid = DispatcherPid
    },
    cm:execute_command(Uid, CommandContext).

%%--------------------------------------------------------------------
%% @doc
%% Display player hp status.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_hp(CommandContext, State, StateName) -> {ok, UpdatedStateName, UpdatedState} when
    CommandContext :: #command_context{},
    State :: #player_state{},
    StateName :: player_fsm:player_state_name(),
    UpdatedStateName :: StateName,
    UpdatedState :: State.
show_hp(
    #command_context{
        dispatcher_pid = DispatcherPid
    },
    #player_state{
        self = #player_profile{
            battle_status = #battle_status{
                'Hp' = Hp,
                'M_hp' = MaxHp
            }
        }
    } = State,
    StateName
) ->
    Message = [<<"hp: ">>, Hp, <<" / ">>, MaxHp, <<"\n">>],
    UpdatedState = player_fsm:do_response_content(State, Message, DispatcherPid),
    {ok, StateName, UpdatedState}.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================