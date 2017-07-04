%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Player gen_statem. This gen_statem is created when player logs in and
%%% detroyed when player logs out. It holds the current player state
%%% and state data for player journey.
%%%
%%% @end
%%% Created : 18. Aug 2015 8:57 PM
%%%-------------------------------------------------------------------
-module(player_statem).
-author("shuieryin").

-behaviour(gen_statem).

%% API
-export([
    start_link/2,
    logout/1,
    get_lang/1,
    response_content/3,
    do_response_content/3,
    do_response_image/3,
    current_scene_name/1,
    append_message/3,
    append_message_local/3,
    simple_player/1,
    non_battle/3,
    update_nls/3,
    player_state/1,
    player_state_by_id/1,
    lang_map/1,
    mail_box/1,
    pending_update_runtime_data/2,
    player_id/1,
    upgrade_value_by_id/2,
    affair_menu/3,
    handle_affair_input/3,
    affair_name/1
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    format_status/2,
    callback_mode/0
]).

-type uid() :: atom().
-type born_month() :: 1..12.
-type gender() :: male | female.
-type id() :: binary(). % define id to binary for convenience of pattern matching.
-type name() :: nls_server:nls_object().
-type mail_object() :: [nls_server:nls_object()].
-type command_func() :: atom(). % generic atom
-type affair_name() :: module() | atom() | binary().
-type affair_data() :: term().

-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").
-include("../data_type/scene_info.hrl").

-type mail_type() :: battle | scene | other.
-type skill_id() :: binary().
-type skill_map() :: #{skill_id() => #skill{}}.
-type skills() :: punch | kick.

-type command_args() :: #perform_args{} | term(). % generic term
-type player_state_name() :: non_battle | affair_menu.

-export_type([
    born_month/0,
    uid/0,
    gender/0,
    id/0,
    name/0,
    skill_id/0,
    command_args/0,
    skill_map/0,
    mail_object/0,
    player_state_name/0,
    command_func/0,
    skills/0,
    affair_name/0,
    affair_data/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a player gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function starts gen_statem by setting player uid as fsm name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Uid, DispatcherPid) -> gen:start_ret() when
    Uid :: uid(),
    DispatcherPid :: pid().
start_link(Uid, DispatcherPid) ->
    gen_statem:start_link({local, Uid}, ?MODULE, {Uid, DispatcherPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Get the current scene name of one player.
%%
%% @end
%%--------------------------------------------------------------------
-spec current_scene_name(PlayerUid) -> scene_statem:scene_name() when
    PlayerUid :: player_statem:uid().
current_scene_name(PlayerUid) ->
    gen_statem:call(PlayerUid, current_scene_name).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the current language of the player.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_lang(Uid) -> Lang when
    Uid :: uid(),
    Lang :: nls_server:support_lang().
get_lang(Uid) ->
    gen_statem:call(Uid, get_lang).

%%--------------------------------------------------------------------
%% @doc
%% Given "ContentList" contains items {nls, NlsKey} with direct return
%% content values, the function is to replace {nls, NlsKey} with the actual
%% nls content, and then immediately return the result to user.
%%
%% This function is called only when is the player language is not given,
%% and the purpose of calling this function is to save return back round
%% by calling get_lang/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec response_content(Uid, ContentList, DispatcherPid) -> ok when
    Uid :: uid(),
    ContentList :: [term()], % generic term
    DispatcherPid :: pid().
response_content(Uid, ContentList, DispatcherPid) ->
    gen_statem:cast(Uid, {response_content, ContentList, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Logs out player by exit the current scene and terminate its player
%% gen_statem process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(Uid) -> ok when
    Uid :: uid().
logout(Uid) ->
    gen_statem:call(Uid, logout).

%%--------------------------------------------------------------------
%% @doc
%% Append message to mailbox. The appended message will be shown to
%% the target player by his/her next action.
%%
%% @end
%%--------------------------------------------------------------------
-spec append_message(Uid, Message, MailType) -> ok when
    Uid :: player_statem:uid(),
    Message :: mail_object(),
    MailType :: mail_type().
append_message(Uid, Message, MailType) ->
    gen_statem:cast(Uid, {append_message, Message, MailType}).

%%--------------------------------------------------------------------
%% @doc
%% Append message.
%%
%% @end
%%--------------------------------------------------------------------
-spec append_message_local(Message, MailType, State) -> UpdatedState when
    Message :: mail_object(),
    MailType :: mail_type(),
    State :: #player_state{},
    UpdatedState :: State.
append_message_local(
    Message,
    battle,
    #player_state{
        mail_box = #mailbox{
            scene = SceneMessages
        } = MailBox
    } = State
) ->
    UpdatedSceneMessages = [Message | SceneMessages],
    State#player_state{
        mail_box = MailBox#mailbox{
            scene = UpdatedSceneMessages
        }
    };
append_message_local(
    Message,
    scene,
    #player_state{
        mail_box = #mailbox{
            scene = SceneMessages
        } = MailBox
    } = State
) ->
    UpdatedSceneMessages = [Message | SceneMessages],
    State#player_state{
        mail_box = MailBox#mailbox{
            scene = UpdatedSceneMessages
        }
    }.
%%append_message_local(
%%    Message,
%%    other,
%%    #player_state{
%%        mail_box = #mailbox{
%%            scene = SceneMessages
%%        } = MailBox
%%    } = State
%%) ->
%%    UpdatedSceneMessages = [Message | SceneMessages],
%%    State#player_state{
%%        mail_box = MailBox#mailbox{
%%            scene = UpdatedSceneMessages
%%        }
%%    }.

%%--------------------------------------------------------------------
%% @doc
%% Returns simple player record.
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_player(PlayerProfile) -> SimplePlayer when
    PlayerProfile :: #player_profile{},
    SimplePlayer :: #simple_player{}.
simple_player(
    #player_profile{
        uid = Uid,
        name = Name,
        id = Id,
        self_description = SelfDescription
    }
) ->
    #simple_player{
        uid = Uid,
        name = Name,
        id = Id,
        character_description = SelfDescription
    }.

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for nls_server:response_content/3.
%% @see nls_server:response_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_response_content(State, NlsObjectList, DispatcherPid) -> ClearedMailBoxState when
    State :: #player_state{},
    NlsObjectList :: [nls_server:nls_object()],
    DispatcherPid :: pid(),
    ClearedMailBoxState :: #player_state{}.
do_response_content(
    #player_state{
        lang_map = LangMap,
        mail_box = #mailbox{
            scene = SceneMessages
        } = MailBox
    } = State,
    NlsObjectList,
    DispatcherPid
) ->
    FinalNlsObjectList = case SceneMessages of
                             [] ->
                                 NlsObjectList;
                             _SceneMessages ->
                                 lists:flatten([lists:reverse(SceneMessages), <<"\n">>, NlsObjectList])
                         end,
    ok = nls_server:do_response_content(LangMap, FinalNlsObjectList, DispatcherPid),
    State#player_state{
        mail_box = MailBox#mailbox{
            scene = []
        }
    }.

%%--------------------------------------------------------------------
%% @doc
%% Response image to user
%%
%% @end
%%--------------------------------------------------------------------
-spec do_response_image(State, ImageUrl, DispatcherPid) -> UpdatedState when
    State :: #player_state{},
    ImageUrl :: binary(),
    DispatcherPid :: pid(),
    UpdatedState :: #player_state{}.
do_response_image(
    State,
    ImageUrl,
    DispatcherPid
) ->
    ok = command_dispatcher:return_content(DispatcherPid, {image, ImageUrl}),
    State.

%%--------------------------------------------------------------------
%% @doc
%% Update language map on the fly. Including newly added, modified,
%% and removed nls. For the ones that removed but still remain in
%% player mail box, will replace them with the actual nls value before
%% the nls map is updated in player state.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_nls(PlayerUid, ChangedLangMap, RemovedNlsSet) -> ok when
    PlayerUid :: uid(),
    ChangedLangMap :: nls_server:lang_map(),
    RemovedNlsSet :: gb_sets:set(atom()). % generic atom
update_nls(PlayerUid, ChangedLangMap, RemovedNlsSet) ->
    gen_statem:call(PlayerUid, {update_nls, ChangedLangMap, RemovedNlsSet}).

%%--------------------------------------------------------------------
%% @doc
%% Update language map on the fly. Including newly added, modified,
%% and removed nls. For the ones that removed but still remain in
%% player mail box, will replace them with the actual nls value before
%% the nls map is updated in player state.
%%
%% @end
%%--------------------------------------------------------------------
-spec pending_update_runtime_data(PlayerUid, {ChangedFilesStruct, DeletedFilesStruct}) -> ok when
    PlayerUid :: uid(),
    ChangedFilesStruct :: [csv_to_object:csv_data_struct()],
    DeletedFilesStruct :: ChangedFilesStruct.
pending_update_runtime_data(PlayerUid, {ChangedFilesStruct, DeletedFilesStruct}) ->
    gen_statem:call(PlayerUid, {pending_update_runtime_data, {ChangedFilesStruct, DeletedFilesStruct}}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve target player state.
%%
%% @end
%%--------------------------------------------------------------------
-spec player_state(PlayerUid) -> PlayerState when
    PlayerUid :: uid(),
    PlayerState :: #player_state{}.
player_state(PlayerUid) ->
    gen_statem:call(PlayerUid, player_state).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve target player state.
%%
%% @end
%%--------------------------------------------------------------------
-spec player_state_by_id(PlayerId) -> PlayerState when
    PlayerId :: id(),
    PlayerState :: #player_state{} | undefined.
player_state_by_id(PlayerId) ->
    case login_server:uids_by_ids([PlayerId]) of
        [PlayerUid] ->
            player_state(PlayerUid);
        _NotFound ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve target player id.
%%
%% @end
%%--------------------------------------------------------------------
-spec player_id(uid()) -> id().
player_id(PlayerUid) ->
    gen_statem:call(PlayerUid, player_id).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve target player language map.
%%
%% @end
%%--------------------------------------------------------------------
-spec lang_map(PlayerUid) -> LangMap when
    PlayerUid :: uid(),
    LangMap :: nls_server:lang_map().
lang_map(PlayerUid) ->
    gen_statem:call(PlayerUid, lang_map).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve target player mail box.
%%
%% @end
%%--------------------------------------------------------------------
-spec mail_box(PlayerUid) -> Mailbox when
    PlayerUid :: uid(),
    Mailbox :: #mailbox{}.
mail_box(PlayerUid) ->
    gen_statem:call(PlayerUid, mail_box).

%%--------------------------------------------------------------------
%% @doc
%% Upgrade player values by integer.
%%
%% @end
%%--------------------------------------------------------------------
-spec upgrade_value_by_id(id(), integer()) -> ok.
upgrade_value_by_id(PlayerId, Value) ->
    case login_server:uids_by_ids([PlayerId]) of
        [PlayerUid] ->
            gen_statem:cast(PlayerUid, {upgrade_value_by_id, Value});
        _NotFound ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle affair menu input.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_affair_input(DispatcherPid, Uid, RawInput) -> ok when
    DispatcherPid :: pid(),
    Uid :: uid(),
    RawInput :: binary().
handle_affair_input(DispatcherPid, Uid, RawInput) ->
    gen_statem:cast(Uid, {handle_affair_input, DispatcherPid, RawInput}).

-spec affair_name(uid()) -> affair_name().
affair_name(Uid) ->
    gen_statem:call(Uid, affair_name).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% See start_link/1 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec init({Uid, DispatcherPid}) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Uid :: uid(),
    DispatcherPid :: pid(),
    StateName :: player_state_name(),
    StateData :: #player_state{},
    Reason :: term(). % generic term
init({Uid, DispatcherPid}) ->
    #player_profile{
        scene = CurSceneName,
        lang = Lang,
        born_month = BornMonth
    } = PlayerProfile = redis_client_server:get(Uid),

    GrabSkillConstraintFunc =
        fun(BornTypeInfoMap) ->
            #{
                BornMonth := #born_type_info{
                    skill = Skills
                }
            } = BornTypeInfoMap,
            [{skill, Skills}]
        end,

    #{
        born_type_info := #{
            BornMonth := #born_type_info{
                skill = SkillKeys
            }
        }
    } = RuntimeDataMap = common_server:runtime_datas([{born_type_info, [BornMonth], GrabSkillConstraintFunc}]),

    State = #player_state{
        self = PlayerProfile,
        lang_map = nls_server:lang_map(Lang),
        mail_box = #mailbox{},
        runtime_data = RuntimeDataMap,
        runtime_data_constraints = [{born_type_info, [BornMonth]}, {skill, SkillKeys}]
    },

    ok = scene_statem:enter(CurSceneName, DispatcherPid, simple_player(PlayerProfile), undefined),
    {ok, non_battle, State}.

%%--------------------------------------------------------------------
%% @doc
%% Refer to below functions for details.
%%
%% @see general_target/3.
%% @see execute_command/2.
%% @see logout/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec non_battle(EventType, EventContent, Data) -> StateFunctionResult when

    EventType :: gen_statem:event_type(),

    EventContent ::
    {'$gen_event', {execute_command | general_target, CommandContext}} |
    logout |
    {response_content, NlsObjectList, DispatcherPid} |
    {append_message, Message, MailType} |
    {upgrade_value_by_id, Value} |
    get_lang |
    player_state |
    current_scene_name |
    {update_nls, ChangedLangMap, RemovedNlsSet} |
    {pending_update_runtime_data, {ChangedFilesStruct, DeletedFilesStruct}} |
    lang_map |
    mail_box |
    player_id |
    affair_name,

    From :: gen_statem:from(),

    Reply :: Lang |
    scene_statem:scene_name() |
    ok |
    Data |
    LangMap |
    PlayerId,

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data, {reply, From, Reply}},

    Action :: gen_statem:reply_action() | {reply, From, Reply},
    NlsObjectList :: [nls_server:nls_object()],
    DispatcherPid :: pid(),
    Message :: mail_object(),
    MailType :: mail_type(),
    Value :: integer(),
    Lang :: nls_server:support_lang(),
    PlayerId :: id(),
    ChangedLangMap :: nls_server:lang_map(),
    RemovedNlsSet :: gb_sets:set(atom()), % generic atom
    LangMap :: nls_server:lang_map(),
    ChangedFilesStruct :: [csv_to_object:csv_data_struct()],
    DeletedFilesStruct :: [csv_to_object:csv_data_struct()],

    CommandContext :: #command_context{},

    Data :: #player_state{},
    State :: gen_statem:state().
non_battle(cast, {response_content, NlsObjectList, DispatcherPid}, Data) ->
    UpdatedData = do_response_content(Data, NlsObjectList, DispatcherPid),
    {next_state, non_battle, UpdatedData};
non_battle(info, {'$gen_event',
    {
        general_target,
        CommandContext
    }}, PlayerState) ->
    general_target(CommandContext, PlayerState, non_battle);
non_battle(info, {'$gen_event',
    {
        execute_command,
        CommandContext
    }}, PlayerState) ->
    execute_command(CommandContext, PlayerState, non_battle);
non_battle({call, From}, logout, PlayerState) ->
    logout(PlayerState, From);
non_battle(cast, {append_message, Message, MailType}, Data) ->
    {next_state, non_battle, append_message_local(Message, MailType, Data)};
non_battle(cast, {upgrade_value_by_id, Value}, PlayerState) ->
    upgrade_value_by_id(Value, non_battle, PlayerState);
non_battle(
    {call, From},
    get_lang,
    #player_state{
        self = #player_profile{
            lang = Lang
        }
    }
) ->
    {keep_state_and_data, {reply, From, Lang}};
non_battle(
    {call, From},
    current_scene_name,
    #player_state{
        self = #player_profile{
            scene = CurrentSceneName
        }
    }
) ->
    {keep_state_and_data, {reply, From, CurrentSceneName}};
non_battle(
    {call, From},
    {update_nls, ChangedLangMap, RemovedNlsSet},
    #player_state{
        lang_map = OldLangMap,
        mail_box = OldMailboxRecord
    } = Data
) ->
    UpdatedOldLangMap = maps:without(gb_sets:to_list(RemovedNlsSet), OldLangMap),
    NewLangMap = maps:merge(UpdatedOldLangMap, ChangedLangMap),
    [MailboxRecordName | OldMailboxes] = tuple_to_list(OldMailboxRecord),
    UpdatedMailboxes = [nls_server:convert_target_nls(OldMailbox, OldLangMap, RemovedNlsSet, []) || OldMailbox <- OldMailboxes],
    {
        next_state,
        non_battle,
        Data#player_state{
            lang_map = NewLangMap,
            mail_box = list_to_tuple([MailboxRecordName | UpdatedMailboxes])
        },
        {reply, From, ok}
    };
non_battle(
    {call, From},
    {pending_update_runtime_data, {ChangedFilesStruct, DeletedFilesStruct}},
    Data
) ->
    {
        next_state,
        non_battle,
        Data#player_state{
            pending_update_runtime_data = {ChangedFilesStruct, DeletedFilesStruct}
        },
        {reply, From, ok}
    };
non_battle({call, From}, player_state, Data) ->
    {keep_state_and_data, {reply, From, Data}};
non_battle({call, From}, lang_map, #player_state{
    lang_map = LangMap
}) ->
    {keep_state_and_data, {reply, From, LangMap}};
non_battle({call, From}, mail_box, #player_state{
    mail_box = Mailbox
}) ->
    {keep_state_and_data, {reply, From, Mailbox}};
non_battle({call, From}, player_id, #player_state{
    self = #player_profile{
        id = PlayerId
    }
}) ->
    {keep_state_and_data, {reply, From, PlayerId}};
non_battle({call, From}, affair_name, #player_state{current_affair = {AffairName, _AffairData}}) ->
    {keep_state_and_data, {reply, From, AffairName}}.

%%--------------------------------------------------------------------
%% @doc
%% Affair finite state machine.
%%
%% @end
%%--------------------------------------------------------------------
-spec affair_menu(EventType, EventContent, Data) -> StateFunctionResult when
    EventType :: gen_statem:event_type(),

    EventContent :: {response_content, NlsObjectList, DispatcherPid} |
    {handle_affair_input, DispatcherPid, RawInput} |
    {upgrade_value_by_id, Value} |
    get_lang | lang_map | player_id | current_scene_name | player_state | mail_box |
    logout | affair_name |
    {'$gen_event', {execute_command | general_target, CommandContext}},

    StateFunctionResult :: gen_statem:event_handler_result(Data) |
    {keep_state_and_data, Action} |
    {next_state, State, Data},

    Value :: integer(),
    CommandContext :: #command_context{},
    RawInput :: binary(),
    DispatcherPid :: pid(),
    NlsObjectList :: [nls_server:nls_object()],
    Action :: gen_statem:action(),
    Data :: #player_state{},
    State :: gen_statem:state().
affair_menu(cast, {response_content, NlsObjectList, DispatcherPid}, Data) ->
    UpdatedData = do_response_content(Data, NlsObjectList, DispatcherPid),
    {next_state, affair_menu, UpdatedData};
affair_menu(cast, {handle_affair_input, DispatcherPid, RawInput}, #player_state{
    current_affair = {AffairModName, _AffairData}
} = PlayerState) ->
    AffairModName:handle_affair_input(PlayerState, DispatcherPid, RawInput);
affair_menu(info, {'$gen_event',
    {
        general_target,
        CommandContext
    }}, PlayerState) ->
    general_target(CommandContext, PlayerState, affair_menu);
affair_menu(info, {'$gen_event',
    {
        execute_command,
        CommandContext
    }}, PlayerState) ->
    execute_command(CommandContext, PlayerState, affair_menu);
affair_menu(
    {call, From},
    get_lang,
    #player_state{
        self = #player_profile{
            lang = Lang
        }
    }
) ->
    {keep_state_and_data, {reply, From, Lang}};
affair_menu(
    {call, From},
    current_scene_name,
    #player_state{
        self = #player_profile{
            scene = CurrentSceneName
        }
    }
) ->
    {keep_state_and_data, {reply, From, CurrentSceneName}};
affair_menu({call, From}, lang_map, #player_state{
    lang_map = LangMap
}) ->
    {keep_state_and_data, {reply, From, LangMap}};
affair_menu({call, From}, player_id, #player_state{
    self = #player_profile{
        id = PlayerId
    }
}) ->
    {keep_state_and_data, {reply, From, PlayerId}};
affair_menu({call, From}, player_state, Data) ->
    {keep_state_and_data, {reply, From, Data}};
affair_menu({call, From}, mail_box, #player_state{
    mail_box = Mailbox
}) ->
    {keep_state_and_data, {reply, From, Mailbox}};
affair_menu(cast, {upgrade_value_by_id, Value}, PlayerState) ->
    upgrade_value_by_id(Value, affair_menu, PlayerState);
affair_menu({call, From}, logout, PlayerState) ->
    logout(PlayerState, From);
affair_menu({call, From}, affair_name, #player_state{current_affair = {AffairName, _AffairData}}) ->
    {keep_state_and_data, {reply, From, AffairName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored. If the gen_statem is terminated
%% abnormally, it is restarted with the current state name and state data. test
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, StateName, StateData) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(), % generic term
    StateName :: player_state_name(),
    StateData :: #player_state{}.
terminate(
    Reason,
    _StateName,
    #player_state{
        self = #player_profile{
            uid = Uid
        } = PlayerProfile
    }
) ->
    case Reason of
        normal ->
            ok;
        _Reason ->
            true = redis_client_server:set(Uid, PlayerProfile, true),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData} when
    OldVsn :: term() | {down, term()}, % generic term
    StateName :: player_state_name(),
    StateData :: #player_state{} | tuple(), % generic tuple
    Extra :: term(), % generic term
    NextStateName :: StateName,
    NewStateData :: StateData.
code_change(_OldVsn, StateName, State, _Extra) ->
    try
        #player_state{
            runtime_data = OldRuntimeDataMap,
            pending_update_runtime_data = {ChangedFilesStruct, DeletedFilesStruct},
            runtime_data_constraints = OldRuntimeDataConstraints,
            self = PlayerProfile
        } = UpdatedState = temp_player_data_update(State),
        {UpdatedRuntimeDataMap, UpdatedRuntimeDataConstraints} =
            case ChangedFilesStruct of
                [] ->
                    {OldRuntimeDataMap, OldRuntimeDataConstraints};
                _DataChanged ->
                    RuntimeDataConstraints = code_change_runtime_constraints(ChangedFilesStruct, OldRuntimeDataConstraints, PlayerProfile),

                    UpdatedOldRuntimeDataMap =
                        lists:foldl(
                            fun
                                ({DataKey, RecordKeys}, AccOldRuntimeDataMap) ->
                                    OldRuntimeData = maps:get(DataKey, AccOldRuntimeDataMap),

                                    AccOldRuntimeDataMap#{
                                        DataKey := maps:without(RecordKeys, OldRuntimeData)
                                    };
                                (DataKey, AccOldRuntimeDataMap) ->
                                    maps:remove(DataKey, AccOldRuntimeDataMap)
                            end, OldRuntimeDataMap, DeletedFilesStruct),

                    FilteredChangedFilesStruct =
                        lists:foldl(
                            fun({DataKey, RecordKeys}, AccFilteredChangedList) ->
                                case lists:keyfind(DataKey, 1, RuntimeDataConstraints) of
                                    false ->
                                        [{DataKey, RecordKeys} | AccFilteredChangedList];
                                    {DataKey, ConstraintRecordKeys} ->
                                        case [ConstraintRecordKey || ConstraintRecordKey <- ConstraintRecordKeys, lists:member(ConstraintRecordKey, RecordKeys)] of
                                            [] ->
                                                AccFilteredChangedList;
                                            FilteredRecordKeys ->
                                                [{DataKey, FilteredRecordKeys} | AccFilteredChangedList]
                                        end
                                end
                            end, [], ChangedFilesStruct),

                    NewRuntimeDataMap =
                        case FilteredChangedFilesStruct of
                            [] ->
                                UpdatedOldRuntimeDataMap;
                            _ChangeOccurs ->
                                ChangedRuntimeDataMap = common_server:runtime_datas(FilteredChangedFilesStruct),
                                maps:fold(
                                    fun(DataKey, ChangedRuntimeData, AccNewRuntimeDataMap) ->
                                        AccNewRuntimeDataMap#{
                                            DataKey := maps:merge(maps:get(DataKey, AccNewRuntimeDataMap), ChangedRuntimeData)
                                        }
                                    end, UpdatedOldRuntimeDataMap, ChangedRuntimeDataMap)
                        end,
                    {NewRuntimeDataMap, RuntimeDataConstraints}
            end,
        {ok, StateName, UpdatedState#player_state{
            runtime_data = UpdatedRuntimeDataMap,
            pending_update_runtime_data = undefined,
            runtime_data_constraints = UpdatedRuntimeDataConstraints
        }}
    catch
        Type:Reason ->
            error_logger:error_msg("Type:~p~nReason:~p~nStackTrace:~p~n", [Type, Reason, erlang:get_stacktrace()]),
            {ok, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is useful for customising the form and
%% appearance of the gen_server status for these cases.
%%
%% @spec format_status(Opt, StatusData) -> Status
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt, StatusData) -> Status when
    Opt :: 'normal' | 'terminate',
    StatusData :: [term()],
    Status :: term(). % generic term
format_status(_Opt, [_PDict, State, _Data]) ->
    %gen_statem:format_status(Opt, StatusData).
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the callback mode to gen_statem
%%
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Special handling codes for hot code upgrading runtime constraints.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change_runtime_constraints(ChangedFilesStruct, RuntimeDataConstraints, PlayerProfile) -> UpdatedRuntimeDataConstraints when
    ChangedFilesStruct :: [csv_to_object:csv_data_struct()],
    RuntimeDataConstraints :: [csv_to_object:csv_data_struct()],
    PlayerProfile :: #player_profile{},
    UpdatedRuntimeDataConstraints :: RuntimeDataConstraints.
code_change_runtime_constraints(
    ChangedFilesStruct,
    RuntimeDataConstraints,
    #player_profile{
        born_month = BornMonth
    }
) ->
    case lists:keyfind(born_type_info, 1, ChangedFilesStruct) of
        false ->
            RuntimeDataConstraints;
        {born_type_info, ChangedBornMonths} ->
            case lists:member(BornMonth, ChangedBornMonths) of
                true ->
                    #born_type_info{
                        skill = NewSkillConstraints
                    } = common_server:runtime_data(born_type_info, BornMonth),
                    lists:keyreplace(skill, 1, RuntimeDataConstraints, {skill, NewSkillConstraints});
                false ->
                    RuntimeDataConstraints
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Temporary code for handling data change for logged in players.
%%
%% @end
%%--------------------------------------------------------------------
-spec temp_player_data_update(State) -> UpdatedState when
    State :: tuple(), % generic tuple
    UpdatedState :: State.
temp_player_data_update(State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Common execute command
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_command(#command_context{}, PlayerState, StateName) ->
    {next_state, NextStateName, UpdatedPlayerState} when

    StateName :: gen_statem:state_name(),
    NextStateName :: StateName,
    PlayerState :: #player_state{},
    PlayerState :: UpdatedPlayerState.
execute_command(
    #command_context{
        raw_input = RawInput,
        command = CommandModule,
        command_func = CommandFunc,
        dispatcher_pid = DispatcherPid
    } = CommandContext,
    #player_state{
        current_affair = {CurrentAffairName, _AffairData}
    } = PlayerState, StateName
) ->
    {FinalStateName, FinalPlayerState}
        = case CurrentAffairName of
              undefined ->
                  {ok, NextStateName, UpdatedPlayerState} = CommandModule:CommandFunc(CommandContext, PlayerState, StateName),
                  {NextStateName, UpdatedPlayerState};
              _InAffair ->
                  UpdatedPlayerState = do_response_content(PlayerState, [{nls, invalid_command}, RawInput, <<"\n">>], DispatcherPid),
                  {StateName, UpdatedPlayerState}
          end,

    {next_state, FinalStateName, FinalPlayerState}.

%%--------------------------------------------------------------------
%% @doc
%% Common call general target
%%
%% @end
%%--------------------------------------------------------------------
-spec general_target(#command_context{}, PlayerState, StateName) ->
    {next_state, NextStateName, UpdatedPlayerState} when

    StateName :: gen_statem:state_name(),
    NextStateName :: StateName,
    PlayerState :: #player_state{},
    PlayerState :: UpdatedPlayerState.
general_target(
    #command_context{
        raw_input = RawInput,
        target_name = TargetId,
        self_targeted_message = SelfMessage,
        dispatcher_pid = DispatcherPid
    } = CommandContext,
    #player_state{
        self = #player_profile{
            scene = CurSceneName,
            id = SrcPlayerId
        } = PlayerProfile,
        current_affair = {CurrentAffairName, _AffairData}
    } = PlayerState, StateName
) ->
    {FinalStateName, FinalPlayerState}
        = case CurrentAffairName of
              undefined ->
                  UpdatedPlayerState =
                      if
                          SrcPlayerId == TargetId ->
                              do_response_content(PlayerState, SelfMessage, DispatcherPid);
                          true ->
                              scene_statem:general_target(CommandContext#command_context{
                                  scene = CurSceneName,
                                  from = simple_player(PlayerProfile)
                              }),
                              PlayerState
                      end,
                  {StateName, UpdatedPlayerState};
              _InAffair ->
                  UpdatedPlayerState = do_response_content(PlayerState, [{nls, invalid_command}, RawInput, <<"\n">>], DispatcherPid),
                  {StateName, UpdatedPlayerState}
          end,
    {next_state, FinalStateName, FinalPlayerState}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrade value by player id
%%
%% @end
%%--------------------------------------------------------------------
-spec upgrade_value_by_id(Value :: integer(), StateName, PlayerState) -> {next_state, StateName, PlayerState} when
    StateName :: gen_statem:state(),
    PlayerState :: #player_state{}.
upgrade_value_by_id(Value, StateName, #player_state{
    self = #player_profile{
        battle_status = #battle_status{
            'Strength' = Strength,
            'Defense' = Defense,
            'Hp' = Hp,
            'Dexterity' = Dexterity
        } = BattleStatus
    } = PlayerProfile
} = PlayerState) ->
    {next_state, StateName, PlayerState#player_state{
        self = PlayerProfile#player_profile{
            battle_status = BattleStatus#battle_status{
                'Strength' = Strength + Value,
                'Defense' = Defense + Value,
                'Hp' = Hp + Value,
                'Dexterity' = Dexterity + Value
            }
        }
    }}.

%%--------------------------------------------------------------------
%% @doc
%% Logout player
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(PlayerState, From) -> {stop, normal, PlayerState} when
    PlayerState :: #player_state{},
    From :: gen_statem:from().
logout(#player_state{
    self = #player_profile{
        uid = Uid,
        scene = CurSceneName
    } = PlayerProfile
} = PlayerState, From) ->
    scene_statem:leave(CurSceneName, Uid),
    true = redis_client_server:set(Uid, PlayerProfile, true),
    error_logger:info_msg("Logout PlayerProfile:~p~n", [PlayerProfile]),
    gen_statem:reply(From, ok),
    {stop, normal, PlayerState}.