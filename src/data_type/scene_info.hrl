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
    npcs :: [npc_statem_manager:npc_spec()]
}).

-record(command_context, {
    raw_input :: binary(),
    command = ?MODULE :: module() | undefined,
    command_func :: player_statem:command_func() | undefined,
    command_args :: player_statem:command_args() | undefined,
    scene :: scene_fsm:scene_name() | undefined,
    from :: scene_fsm:scene_object() | undefined,
    to :: scene_fsm:scene_object() | undefined,
    dispatcher_pid :: pid() | undefined,
    target_name :: player_statem:id() | npc_statem:npc_id() | undefined,
    sequence :: non_neg_integer() | undefined,
    target_name_bin :: binary() | undefined,
    self_targeted_message :: [nls_server:nls_object()] | undefined,
    affair_mod_name :: module()
}).