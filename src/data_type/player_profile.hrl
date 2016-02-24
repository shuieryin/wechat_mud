%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 11:33 AM
%%%-------------------------------------------------------------------
-author("shuieryin").

-ifndef(BATTLE_STATUS).
-include("../data_type/battle.hrl").
-define(BATTLE_STATUS, 1).
-endif.

-include("../data_type/skill.hrl").

-record(avatar_profile, {
    name :: player_fsm:name(),
    description :: [nls_server:nls_object()]
}).

-record(born_type_info, {
    born_type :: player_fsm:born_month(),
    strength :: integer(),
    defense :: integer(),
    hp :: integer(),
    dexterity :: integer(),
    skill :: [player_fsm:skill_id()]
}).

-record(player_profile, {
    uid :: player_fsm:uid(),
    id :: player_fsm:id(),
    name :: player_fsm:name(),
    description :: nls_server:nls_object(),
    self_description :: nls_server:nls_object(),
    born_month :: player_fsm:born_month(),
    gender :: player_fsm:gender(),
    lang :: nls_server:support_lang(),
    register_time :: pos_integer(),
    scene :: scene_fsm:scene_name(),
    avatar_profile :: #avatar_profile{},
    battle_status :: #battle_status{}
}).

-record(simple_player, {
    uid :: player_fsm:uid(),
    name :: player_fsm:name(),
    id :: player_fsm:id(),
    character_description :: nls_server:nls_object()
}).

-record(mailbox, {
    battle = [] :: [player_fsm:mail_object()],
    scene = [] :: [player_fsm:mail_object()],
    other = [] :: [player_fsm:mail_object()]
}).

-record(player_state, {
    self :: #player_profile{},
    mail_box :: #mailbox{},
    lang_map :: nls_server:lang_map(),
    runtime_data :: csv_to_object:csv_object(),
    battle_status_ri = record_info(fields, battle_status) :: [atom()], % generic atom % battle status record info
    pending_update_runtime_data :: {[csv_to_object:csv_data_struct()], [csv_to_object:csv_data_struct()]},
    runtime_data_constraints :: [csv_to_object:csv_data_struct()]
}).