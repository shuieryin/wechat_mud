%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Player gen_fsm. This gen_fsm is created when player logs in and
%%% detroyed when player logs out. It holds the current player state
%%% and state data for player journey.
%%%
%%% @end
%%% Created : 18. Aug 2015 8:57 PM
%%%-------------------------------------------------------------------
-module(player_fsm).
-author("shuieryin").

-behaviour(gen_fsm).

%% API
-export([
    start_link/2,
    start/2,
    logout/1,
    get_lang/1,
    response_content/3,
    do_response_content/3,
    current_scene_name/1,
    append_message/3,
    append_message_local/3,
    simple_player/1,
    non_battle/2,
    non_battle/3,
    battle/2
]).

%% gen_fsm callbacks
-export([
    init/1,
    state_name/2,
    state_name/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    format_status/2
]).

-type uid() :: atom().
-type born_month() :: 1..12.
-type gender() :: male | female.
-type id() :: binary(). % define id to binary for convenience of pattern matching.
-type name() :: nls_server:nls_object().
-type mail_object() :: [nls_server:nls_object()].
-type command_func() :: atom(). % generic atom

-include("../data_type/player_profile.hrl").
-include("../data_type/npc_profile.hrl").
-include("../data_type/scene_info.hrl").

-type mail_type() :: battle | scene | other.
-type skill_id() :: binary().
-type skill_map() :: #{skill_id() => #skill{}}.
-type skills() :: attack.

-type command_args() :: #perform_args{} | term(). % generic term
-type player_state_name() :: battle | non_battle | state_name.

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
    skills/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a player gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% This function starts gen_fsm by setting player uid as fsm name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Uid, DispatcherPid) -> gen:start_ret() when
    Uid :: uid(),
    DispatcherPid :: pid().
start_link(Uid, DispatcherPid) ->
    gen_fsm:start_link({local, Uid}, ?MODULE, {Uid, DispatcherPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Same as start_link/1 but without link.
%% @see start_link/0.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Uid, DispatcherPid) -> gen:start_ret() when
    Uid :: uid(),
    DispatcherPid :: pid().
start(Uid, DispatcherPid) ->
    gen_fsm:start({local, Uid}, ?MODULE, {Uid, DispatcherPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Get the current scene name of one player.
%%
%% @end
%%--------------------------------------------------------------------
-spec current_scene_name(PlayerUid) -> scene_fsm:scene_name() when
    PlayerUid :: player_fsm:uid().
current_scene_name(PlayerUid) ->
    gen_fsm:sync_send_all_state_event(PlayerUid, current_scene_name).

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
    gen_fsm:sync_send_all_state_event(Uid, get_lang).

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
    gen_fsm:send_all_state_event(Uid, {response_content, ContentList, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Logs out player by exit the current scene and terminate its player
%% gen_fsm process.
%%
%% @end
%%--------------------------------------------------------------------
-spec logout(Uid) -> ok when
    Uid :: uid().
logout(Uid) ->
    gen_fsm:sync_send_event(Uid, logout).

%%--------------------------------------------------------------------
%% @doc
%% Append message to mailbox. The appended message will be shown to
%% the target player by his/her next action.
%%
%% @end
%%--------------------------------------------------------------------
-spec append_message(Uid, Message, MailType) -> ok when
    Uid :: player_fsm:uid(),
    Message :: mail_object(),
    MailType :: mail_type().
append_message(Uid, Message, MailType) ->
    gen_fsm:send_all_state_event(Uid, {append_message, Message, MailType}).

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
    State#player_state{mail_box = MailBox#mailbox{scene = UpdatedSceneMessages}};
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
    State#player_state{mail_box = MailBox#mailbox{scene = UpdatedSceneMessages}};
append_message_local(
    Message,
    other,
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
                             _ ->
                                 lists:flatten([lists:reverse(SceneMessages), <<"\n">>, NlsObjectList])
                         end,
    ok = nls_server:do_response_content(LangMap, FinalNlsObjectList, DispatcherPid),
    State#player_state{
        mail_box = MailBox#mailbox{
            scene = []
        }
    }.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
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
    #player_profile{scene = CurSceneName, lang = Lang} = PlayerProfile = redis_client_server:get(Uid),
    State = #player_state{
        self = PlayerProfile,
        lang_map = nls_server:get_lang_map(Lang),
        mail_box = #mailbox{},
        skill_map = common_server:get_runtime_data(skill)
    },

    ok = scene_fsm:enter(CurSceneName, DispatcherPid, simple_player(PlayerProfile), undefined),
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
-spec non_battle(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event ::
    {general_target, CommandContext} |
    {execute_command, CommandContext},

    CommandContext :: #command_context{},
    Reason :: logout,

    State :: #player_state{},
    NextStateName :: player_state_name(),
    NextState :: State,
    NewState :: State.
non_battle(
    {
        general_target,
        #command_context{
            target_name = TargetId,
            self_targeted_message = SelfMessage,
            dispatcher_pid = DispatcherPid
        } = CommandContext
    },
    #player_state{
        self = #player_profile{
            scene = CurSceneName,
            id = SrcPlayerId
        } = PlayerProfile
    } = State
) ->
    UpdatedState =
        if
            SrcPlayerId == TargetId ->
                do_response_content(State, SelfMessage, DispatcherPid);
            true ->
                scene_fsm:general_target(CommandContext#command_context{
                    scene = CurSceneName,
                    from = simple_player(PlayerProfile)
                }),
                State
        end,
    {next_state, non_battle, UpdatedState};
non_battle(
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, non_battle),
    {next_state, NextStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @doc
%% Refer to below functions for details.
%%
%% @see logout/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec non_battle(Event, From, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {reply, Reply, NextStateName, NextState} |
    {reply, Reply, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} |
    {stop, Reason, Reply, NewState} when

    Event :: logout,
    Reply :: ok,

    From :: {pid(), term()}, % generic term
    State :: #player_state{},
    NextStateName :: player_state_name(),
    NextState :: State,
    Reason :: normal | term(), % generic term
    NewState :: State.
non_battle(
    logout,
    From,
    #player_state{
        self = #player_profile{
            scene = CurSceneName,
            uid = Uid
        } = PlayerProfile
    } = State
) ->
    scene_fsm:leave(CurSceneName, Uid),
    true = redis_client_server:set(Uid, PlayerProfile, true),
    error_logger:info_msg("Logout PlayerProfile:~p~n", [PlayerProfile]),
    gen_fsm:reply(From, ok),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @doc
%% Refer to below functions for details.
%% @see perform/4.
%%
%% @end
%%--------------------------------------------------------------------
-spec battle(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event ::
    {execute_command, CommandContext},

    CommandContext :: #command_context{},

    State :: #player_state{},
    NextStateName :: player_state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
battle(
    {
        execute_command,
        #command_context{
            command = CommandModule,
            command_func = CommandFunc
        } = CommandContext
    },
    State
) ->
    {ok, NextStateName, UpdatedState} = CommandModule:CommandFunc(CommandContext, State, battle),
    {next_state, NextStateName, UpdatedState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec state_name(Event, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Event :: term(), % generic term
    State :: #player_state{},
    NextStateName :: player_state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term(). % generic term
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec state_name(Event, From, State) ->
    {next_state, NextStateName, NextState} |
    {next_state, NextStateName, NextState, timeout() | hibernate} |
    {reply, Reply, NextStateName, NextState} |
    {reply, Reply, NextStateName, NextState, timeout() | hibernate} |
    {stop, Reason, NewState} |
    {stop, Reason, Reply, NewState} when

    Event :: term(), % generic term
    Reply :: ok,

    From :: {pid(), term()}, % generic term
    State :: #player_state{},
    NextStateName :: player_state_name(),
    NextState :: State,
    Reason :: normal | term(), % generic term
    NewState :: State.
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when

    Event ::
    {response_content, NlsObjectList, DispatcherPid} |
    {append_message, Message, MailType} |
    stop,

    NlsObjectList :: [nls_server:nls_object()],
    DispatcherPid :: pid(),
    Message :: mail_object(),
    MailType :: mail_type(),

    StateName :: player_state_name(),
    StateData :: #player_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_event({response_content, NlsObjectList, DispatcherPid}, StateName, State) ->
    UpdatedState = do_response_content(State, NlsObjectList, DispatcherPid),
    {next_state, StateName, UpdatedState};
handle_event({append_message, Message, MailType}, StateName, State) ->
    {next_state, StateName, append_message_local(Message, MailType, State)};
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(Event, From, StateName, StateData) ->
    {reply, Reply, NextStateName, NewStateData} |
    {reply, Reply, NextStateName, NewStateData, timeout() | hibernate} |
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, Reply, NewStateData} |
    {stop, Reason, NewStateData} when

    Event :: get_lang |
    current_scene_name,

    Reply :: Lang |
    scene_fsm:scene_name(),

    Lang :: nls_server:support_lang(),

    From :: {pid(), Tag :: term()}, % generic term
    StateName :: player_state_name(),
    StateData :: #player_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(). % generic term
handle_sync_event(
    get_lang,
    _From,
    StateName,
    #player_state{
        self = #player_profile{
            lang = Lang
        }
    } = State
) ->
    {reply, Lang, StateName, State};
handle_sync_event(
    current_scene_name,
    _From,
    StateName,
    #player_state{
        self = #player_profile{
            scene = CurrentSceneName
        }
    } = State
) ->
    {reply, CurrentSceneName, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, StateName, StateData) ->
    {next_state, NextStateName, NewStateData} |
    {next_state, NextStateName, NewStateData, timeout() | hibernate} |
    {stop, Reason, NewStateData} when

    Info :: term(), % generic term
    StateName :: player_state_name(),
    StateData :: #player_state{},
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: normal | term(). % generic term
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored. If the gen_fsm is terminated
%% abnormally, it is restarted with the current state name and state data.
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
        _ ->
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
    StateData :: #player_state{},
    Extra :: term(), % generic term
    NextStateName :: StateName,
    NewStateData :: StateData.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

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
    StatusData :: [PDict | State],
    PDict :: [{Key :: term(), Value :: term()}], % generic term
    State :: #player_state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_fsm:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================