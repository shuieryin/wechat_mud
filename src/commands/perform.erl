%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Perform skill on target.
%%%
%%% @end
%%% Created : 10. Jan 2016 8:50 PM
%%%-------------------------------------------------------------------
-module(perform).
-author("shuieryin").

%% API
-export([exec/3]).

-include("../data_type/scene_info.hrl").
-include("../data_type/player_profile.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Show the first matched target scene object description.
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(DispatcherPid, Uid, RestArgsBin) -> ok when
    Uid :: player_fsm:uid(),
    DispatcherPid :: pid(),
    RestArgsBin :: binary().
exec(DispatcherPid, Uid, Args) ->
    [SkillId, TargetArgs] = re:split(Args, <<"\s+on\s+">>),
    {ok, TargetId, Sequence} = cm:parse_target_id(TargetArgs),
    TargetContent = #target_content{
        actions = [perform_target, under_perform, performed],
        action_args = #perform_args{
            skill_id = SkillId
        },
        dispatcher_pid = DispatcherPid,
        target = TargetId,
        sequence = Sequence,
        target_bin = TargetArgs,
        self_targeted_message = [{nls, attack_self}, <<"\n">>]
    },

    cm:target(Uid, TargetContent).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================