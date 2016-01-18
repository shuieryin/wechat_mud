%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Attack target. This will change both players state from
%%% non_battle to battle.
%%%
%%% @end
%%% Created : 16. Jan 2016 8:28 PM
%%%-------------------------------------------------------------------
-module(attack).
-author("shuieryin").

%% API
-export([exec/3]).

-include("../data_type/scene_info.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Attack target. This will change both players state from
%% non_battle to battle.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, TargetArgs) -> ok when
    DispatcherPid :: pid(),
    Uid :: player_fsm:uid(),
    TargetArgs :: binary().
exec(DispatcherPid, Uid, TargetArgs) ->
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    TargetContent = #target_content{
        actions = [under_attack, attacked],
        dispatcher_pid = DispatcherPid,
        target = TargetId,
        sequence = Sequence,
        target_bin = TargetArgs,
        self_targeted_message = [{nls, attack_self}, <<"\n">>]
    },
    cm:general_target(Uid, TargetContent).

%%%===================================================================
%%% Internal functions
%%%===================================================================