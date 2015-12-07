%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 11:33 AM
%%%-------------------------------------------------------------------
-author("shuieryin").

-record(avatar_profile, {
    name :: player_fsm:name(),
    description :: [nls_server:nls_object()]
}).

-record(born_type_info, {
    born_type :: player_fsm:born_month(),
    attack :: integer(),
    defence :: integer(),
    hp :: integer(),
    dexterity :: integer()
}).

-record(player_profile, {
    uid :: player_fsm:uid(),
    id :: player_fsm:id(),
    name :: player_fsm:name(),
    description :: nls_server:nls_object(),
    self_description :: nls_server:nls_object(),
    born_month :: player_fsm:born_month(),
    born_type :: #born_type_info{},
    gender :: player_fsm:gender(),
    lang :: nls_server:support_lang(),
    register_time :: pos_integer(),
    scene :: scene_fsm:scene_name(),
    avatar_profile :: #avatar_profile{} | undefined
}).

-record(simple_player, {
    uid :: player_fsm:uid(),
    name :: player_fsm:name(),
    id :: player_fsm:id(),
    name_description :: nls_server:nls_object()
}).