%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 4:55 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(scene_info, {
    id :: scene_fsm:scene_name(),
    exits :: scene_fsm:exits_map(),
    title :: nls_server:key(),
    desc :: nls_server:key(),
    npcs :: [npc_fsm_manager:npc_spec()]
}).