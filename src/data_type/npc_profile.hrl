%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 3:08 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-ifndef(BATTLE_STATUS).
-include("../data_type/battle.hrl").
-define(BATTLE_STATUS, 1).
-endif.

-record(npc_profile, {
    npc_uid :: npc_fsm:npc_uid(),
    npc_id :: npc_fsm:npc_id(), % define npc_id to binary for convenience of pattern matching.
    npc_name :: nls_server:nls_object(),
    character_desc :: nls_server:nls_object(),
    self_description :: nls_server:nls_object(),
    attack :: integer(),
    defense :: integer(),
    hp :: integer(),
    dexterity :: integer()
}).

-record(simple_npc, {
    npc_uid :: npc_fsm:npc_uid(),
    npc_id :: npc_fsm:npc_id(),
    npc_name :: npc_fsm:npc_name()
}).

-record(npc_state, {
    self :: #npc_profile{},
    battle_status :: #battle_status{}
}).