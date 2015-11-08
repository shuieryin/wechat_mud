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
-export([start_link/1,
    logout/1,
    go_direction/3,
    look_scene/2,
    get_lang/1,
    response_content/4,
    leave_scene/1,
    switch_lang/3,
    look_target/3]).

%% gen_fsm callbacks
-export([init/1,
    state_name/2,
    state_name/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    format_status/2]).

-define(SERVER, ?MODULE).

-type uid() :: atom().
-type born_month() :: 1..12.
-type gender() :: male | female.
-type born_type_info() :: #{born_type => born_month(), attack => integer(), defence => integer(), hp => integer(), dexterity => integer()}.
-type player_profile() :: #{uid => uid(), born_month => born_month, born_type => born_type_info, gender => gender(), lang => nls_server:support_lang(), register_time => pos_integer(), scene => scene_fsm:scene_name()}.
-type simple_player() :: {player, Uid :: uid()}.
-type state() :: #{self => player_profile()}.
-type state_name() :: state_name | non_battle.

-export_type([born_type_info/0,
    player_profile/0,
    simple_player/0,
    born_month/0]).

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
-spec start_link(PlayerProfile) -> {ok, pid()} | ignore | {error, Reason} when
    PlayerProfile :: player_profile(),
    Reason :: term().
start_link(#{uid := Uid} = PlayerProfile) ->
    gen_fsm:start({local, Uid}, ?MODULE, PlayerProfile, []).

%%--------------------------------------------------------------------
%% @doc
%% Go to direction.
%%
%% This function does the followings:
%% 1. Checks if current scene is linked to the target scene, if so go to
%% step 2, otherwise remind user the direction is invalid.
%%
%% 2. Leave the current scene, enter the target scene, and display the
%% target scene info to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec go_direction(Uid, DispatcherPid, Direction) -> ok when
    Uid :: uid(),
    DispatcherPid :: pid(),
    Direction :: direction:directions().
go_direction(Uid, DispatcherPid, Direction) ->
    gen_fsm:send_all_state_event(Uid, {go_direction, DispatcherPid, Direction}).

%%--------------------------------------------------------------------
%% @doc
%% Displays the current scene info to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_scene(Uid, DispatcherPid) -> ok when
    Uid :: uid(),
    DispatcherPid :: pid().
look_scene(Uid, DispatcherPid) ->
    gen_fsm:send_all_state_event(Uid, {look_scene, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Displays the current scene info to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec look_target(Uid, DispatcherPid, LookArgs) -> ok when
    Uid :: uid(),
    DispatcherPid :: pid(),
    LookArgs :: binary().
look_target(Uid, DispatcherPid, LookArgs) ->
    gen_fsm:send_all_state_event(Uid, {look_target, DispatcherPid, LookArgs}).

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
-spec response_content(Uid, NlsServer, ContentList, DispatcherPid) -> ok when
    Uid :: uid(),
    NlsServer :: atom(),
    ContentList :: [term()],
    DispatcherPid :: pid().
response_content(Uid, NlsServer, ContentList, DispatcherPid) ->
    gen_fsm:send_all_state_event(Uid, {response_content, NlsServer, ContentList, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Leaves the current scene.
%%
%% This function is called when player is going to other scene or
%% logging out.
%%
%% @end
%%--------------------------------------------------------------------
-spec leave_scene(Uid) -> ok when
    Uid :: uid().
leave_scene(Uid) ->
    gen_fsm:send_all_state_event(Uid, leave_scene).

%%--------------------------------------------------------------------
%% @doc
%% Switches player language.
%%
%% @end
%%--------------------------------------------------------------------
-spec switch_lang(DispatcherPid, Uid, TargetLang) -> ok when
    DispatcherPid :: pid(),
    Uid :: uid(),
    TargetLang :: nls_server:support_lang().
switch_lang(DispatcherPid, Uid, TargetLang) ->
    gen_fsm:send_all_state_event(Uid, {switch_lang, DispatcherPid, TargetLang}).

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
    gen_fsm:send_all_state_event(Uid, logout).

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
-spec init(PlayerProfile) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    PlayerProfile :: player_profile(),
    StateName :: state_name(),
    StateData :: state(),
    Reason :: term().
init(PlayerProfile) ->
    error_logger:info_msg("Player fsm initialized:~p~n", [PlayerProfile]),
    {ok, non_battle, #{self => PlayerProfile}}.

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

    Event :: term(),
    State :: state(),
    NextStateName :: state_name(),
    NextState :: State,
    NewState :: State,
    Reason :: term().
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

    Event :: term(),
    From :: {pid(), term()},
    State :: state(),
    NextStateName :: state_name(),
    NextState :: State,
    Reason :: normal | term(),
    Reply :: term(),
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
    {go_direction, DispatcherPid, Direction} |
    {look_scene, DispatcherPid} |
    {look_target, DispatcherPid, LookArgs} |
    {response_content, NlsServer, ContentList, DispatcherPid} |
    leave_scene |
    {switch_lang, DispatcherPid, TargetLang} |
    {logout, NotifyOkPid},

    NlsServer :: atom(),
    ContentList :: [term()],
    DispatcherPid :: pid(),
    Direction :: direction:directions(),
    StateName :: state_name(),
    StateData :: state(),
    TargetLang :: nls_server:support_lang(),
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term(),
    LookArgs :: binary(),
    NotifyOkPid :: pid().
handle_event({go_direction, DispatcherPid, Direction}, StateName, #{self := #{scene := CurSceneName, uid := Uid, lang := Lang} = PlayerProfile} = State) ->
    TargetSceneName =
        case scene_fsm:go_direction(CurSceneName, Uid, Lang, DispatcherPid, Direction) of
            undefined ->
                CurSceneName;
            NewSceneName ->
                scene_fsm:enter(NewSceneName, Uid, Lang, DispatcherPid),
                NewSceneName
        end,

    {next_state, StateName, State#{self := PlayerProfile#{scene := TargetSceneName}}};
handle_event({look_scene, DispatcherPid}, StateName, #{self := #{scene := CurSceneName, uid := Uid, lang := Lang}} = State) ->
    scene_fsm:look_scene(CurSceneName, Uid, Lang, DispatcherPid),
    {next_state, StateName, State};
handle_event({look_target, DispatcherPid, LookArgs}, StateName, #{self := #{scene := CurSceneName, uid := Uid, lang := Lang}} = State) ->
    scene_fsm:look_target(CurSceneName, Uid, Lang, DispatcherPid, LookArgs),
    {next_state, StateName, State};
handle_event({response_content, NlsServer, ContentList, DispatcherPid}, StateName, #{self := #{lang := Lang}} = State) ->
    nls_server:response_content(NlsServer, ContentList, Lang, DispatcherPid),
    {next_state, StateName, State};
handle_event(leave_scene, StateName, #{self := #{scene := CurSceneName, uid := Uid}} = State) ->
    scene_fsm:leave(CurSceneName, Uid),
    {next_state, StateName, State};
handle_event({switch_lang, DispatcherPid, TargetLang}, StateName, #{self := #{uid := Uid} = PlayerProfile} = State) ->
    nls_server:response_content(commands, [{nls, lang_switched}], TargetLang, DispatcherPid),
    UpdatedPlayerProfile = PlayerProfile#{lang := TargetLang},
    redis_client_server:async_set(Uid, UpdatedPlayerProfile, true),
    {next_state, StateName, State#{self := UpdatedPlayerProfile}};
handle_event(logout, _StateName, #{self := #{scene := CurSceneName, uid := Uid} = PlayerProfile} = State) ->
    scene_fsm:leave(CurSceneName, Uid),
    error_logger:info_msg("Logout PlayerProfile:~p~n", [PlayerProfile]),
    redis_client_server:set(Uid, PlayerProfile, true),
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

    Event :: get_lang,
    From :: {pid(), Tag :: term()},
    StateName :: state_name(),
    StateData :: state(),
    Reply :: term(),
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: term().
handle_sync_event(get_lang, _From, StateName, #{self := #{lang := Lang}} = State) ->
    {reply, Lang, StateName, State}.

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

    Info :: term(),
    StateName :: state_name(),
    StateData :: state(),
    NextStateName :: StateName,
    NewStateData :: StateData,
    Reason :: normal | term().
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
-spec terminate(Reason, StateName, StateData) -> term() when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    StateName :: state_name(),
    StateData :: state().
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData} when
    OldVsn :: term() | {down, term()},
    StateName :: state_name(),
    StateData :: state(),
    Extra :: term(),
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
    PDict :: [{Key :: term(), Value :: term()}],
    State :: term(),
    Status :: term().
format_status(Opt, StatusData) ->
    gen_fsm:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================