%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 3:08 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(npc_born_info, {
    npc_fsm_id :: npc_fsm_manager:npc_fsm_id(),
    npc_id :: binary(), % define npc_id to binary for convenience of pattern matching.
    name_nls_key :: atom(),
    description_nls_key :: atom(),
    self_description_nls_key :: atom(),
    attack :: integer(),
    defence :: integer(),
    hp :: integer(),
    dexterity :: integer()
}).

-record(simple_npc_fsm, {
    npc_fsm_id :: npc_fsm_manager:npc_fsm_id(),
    npc_type :: npc_fsm_manager:npc_type(),
    npc_name_nls_key :: nls_server:key()
}).

-record(npc_state, {
    npc_profile :: #npc_born_info{}
}).