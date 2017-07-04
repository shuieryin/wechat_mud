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

-record(ask_n_answer, {
    affair_nls_values :: nls_server:nls_object() | [nls_server:value()], % The value of this field will be converted from "nls_server:nls_object()" to "[nls_server:value()]" when initializing the npc it belongs to
    affair_mod :: module(),
    affair_func :: atom()
}).

-record(npc_profile, {
    npc_uid :: npc_statem:npc_uid(),
    npc_id :: npc_statem:npc_id(), % define npc_id to binary for convenience of pattern matching.
    npc_name :: nls_server:nls_object(),
    character_desc :: nls_server:nls_object(),
    self_description :: nls_server:nls_object(),
    ask_n_answers :: [#ask_n_answer{}],
    strength :: integer(),
    defense :: integer(),
    hp :: integer(),
    dexterity :: integer()
}).

-record(simple_npc, {
    npc_uid :: npc_statem:npc_uid(),
    npc_id :: npc_statem:npc_id(),
    npc_name :: npc_statem:npc_name()
}).

-record(npc_state, {
    self :: #npc_profile{},
    battle_status :: #battle_status{},
    battle_status_ri = record_info(fields, battle_status) :: [atom()], % generic atom % battle status record info
    npc_context = #{} :: map()
}).