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
-export([start/1,
    logout/1,
    go_direction/3,
    look_scene/2,
    get_lang/1,
    response_content/4,
    leave_scene/1,
    switch_lang/3]).

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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a player gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% When the first variable in the tuple is "init", it creates a
%% player gen_fsm with player profile and the default state.
%%
%% When the first variable in the tuple is "bringup", it creats
%% a player gen_fsm with state name and state data from the
%% abnormally terminiated player gen_fsm.
%%
%% This functino starts gen_fsm by setting uid as fsm name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Request) -> {ok, pid()} | ignore | {error, Reason :: term()} when
    Request :: {init, PlayerProfile} | {bringup, StateName, StateData},
    StateName :: atom(),
    StateData :: map(),
    PlayerProfile :: command_dispatcher:uid_profile().
start({init, PlayerProfile}) ->
    Uid = maps:get(uid, PlayerProfile),
    gen_fsm:start({local, Uid}, ?MODULE, {init, PlayerProfile}, []);
start({bringup, StateName, StateData}) ->
    #{self := PlayerProfile} = StateData,
    #{uid := Uid} = PlayerProfile,
    ok = common_api:until_process_terminated(Uid, 20),
    gen_fsm:start({local, Uid}, ?MODULE, {bringup, StateName, StateData}, []).

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
    Uid :: atom(),
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
    Uid :: atom(),
    DispatcherPid :: pid().
look_scene(Uid, DispatcherPid) ->
    gen_fsm:send_all_state_event(Uid, {look_scene, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the current language of the player.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_lang(Uid) -> Lang when
    Uid :: atom(),
    Lang :: atom().
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
    Uid :: atom(),
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
    Uid :: atom().
leave_scene(Uid) ->
    gen_fsm:send_all_state_event(Uid, leave_scene).

%%--------------------------------------------------------------------
%% @doc
%% Switches player language.
%%
%% @end
%%--------------------------------------------------------------------
-spec switch_lang(DispatcherPid, Uid, Lang) -> ok when
    DispatcherPid :: pid(),
    Uid :: atom(),
    Lang :: atom().
switch_lang(DispatcherPid, Uid, Lang) ->
    gen_fsm:send_all_state_event(Uid, {switch_lang, DispatcherPid, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Logs out player by exit the current scene and terminate its player
%% gen_fsm process.
%%
%% @end
%%--------------------------------------------------------------------
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
%% See start/1 for details of "init" and "bringup" definitions.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, StateName, StateData} |
    {ok, StateName, StateData, timeout() | hibernate} |
    {stop, Reason} |
    ignore when
    StateName :: atom(),
    StateData :: map(),
    Reason :: term().
init({init, PlayerProfile}) ->
    error_logger:info_msg("Init player:~p~n", [PlayerProfile]),
    {ok, non_battle, #{self => PlayerProfile}};
init({bringup, StateName, StateData}) ->
    error_logger:info_msg("player fsm bringup:~p~n", [StateData]),
    {ok, StateName, StateData}.

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
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    NewState :: map(),
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
    State :: map(),
    NextStateName :: atom(),
    NextState :: map(),
    Reason :: normal | term(),
    Reply :: term(),
    NewState :: map().
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

    Event :: {go_direction, DispatcherPid, Direction} | {look_scene, DispatcherPid} | {response_content, NlsServer, ContentList, DispatcherPid},
    NlsServer :: atom(),
    ContentList :: [term()],
    DispatcherPid :: pid(),
    Direction :: direction:directions(),
    StateName :: atom(),
    StateData :: map(),
    NextStateName :: atom(),
    NewStateData :: map(),
    Reason :: term().
handle_event({go_direction, DispatcherPid, Direction}, StateName, State) ->
    #{self := PlayerProfile} = State,
    #{scene := CurSceneName} = PlayerProfile,

    #{uid := Uid, lang := Lang} = PlayerProfile,
    TargetSceneName = case scene_fsm:leave(CurSceneName, Uid, Direction) of
                          {undefined, NlsServerName} ->
                              nls_server:response_content(NlsServerName, [{nls, invalid_exit}], Lang, DispatcherPid),
                              CurSceneName;
                          NewSceneName ->
                              scene_fsm:enter(NewSceneName, PlayerProfile, DispatcherPid),
                              NewSceneName
                      end,

    {next_state, StateName, State#{self := PlayerProfile#{scene := TargetSceneName}}};
handle_event({look_scene, DispatcherPid}, StateName, State) ->
    #{self := PlayerProfile} = State,
    #{scene := CurSceneName} = PlayerProfile,
    scene_fsm:look(CurSceneName, DispatcherPid, PlayerProfile),
    {next_state, StateName, State};
handle_event({response_content, NlsServer, ContentList, DispatcherPid}, StateName, State) ->
    #{self := PlayerProfile} = State,
    #{lang := Lang} = PlayerProfile,
    nls_server:response_content(NlsServer, ContentList, Lang, DispatcherPid),
    {next_state, StateName, State};
handle_event(leave_scene, StateName, State) ->
    #{self := PlayerProfile} = State,
    #{scene := CurSceneName, uid := Uid} = PlayerProfile,
    scene_fsm:leave(CurSceneName, Uid),
    {next_state, StateName, State};
handle_event({switch_lang, DispatcherPid, RawTargetLang}, StateName, State) ->
    #{self := PlayerProfile} = State,
    #{uid := Uid, lang := CurLang} = PlayerProfile,

    InvalidLangFun = fun() ->
        nls_server:response_content(lang, [{nls, invalid_lang}, RawTargetLang, <<"\n\n">>, {nls, info}], CurLang, DispatcherPid),
        State
    end,

    UpdatedState =
        try
            TargetLang = binary_to_atom(RawTargetLang, utf8),
            case nls_server:is_valid_lang(lang, TargetLang) of
                true ->
                    nls_server:response_content(lang, [{nls, lang_switched}], TargetLang, DispatcherPid),
                    UpdatedPlayerProfile = PlayerProfile#{lang := TargetLang},
                    redis_client_server:async_set(Uid, UpdatedPlayerProfile, true),
                    State#{self := UpdatedPlayerProfile};
                _ ->
                    InvalidLangFun()
            end
        catch
            _:_ ->
                InvalidLangFun()
        end,

    {next_state, StateName, UpdatedState};
handle_event(logout, _StateName, State) ->
    #{self := PlayerProfile} = State,
    #{scene := CurSceneName, uid := Uid} = PlayerProfile,
    scene_fsm:leave(CurSceneName, Uid),

    error_logger:info_msg("logout PlayerProfile:~p~n", [PlayerProfile]),
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
    StateName :: atom(),
    StateData :: term(),
    Reply :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
    Reason :: term().
handle_sync_event(get_lang, _From, StateName, State) ->
    #{self := PlayerProfile} = State,
    {reply, maps:get(lang, PlayerProfile), StateName, State}.

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
    StateName :: atom(),
    StateData :: term(),
    NextStateName :: atom(),
    NewStateData :: term(),
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
    StateName :: atom(),
    StateData :: term().
terminate(Reason, StateName, StateData) ->
    case Reason of
        Result when Result /= normal andalso Result /= done ->
            scene_fsm:bringup_player(StateName, StateData);
        _ ->
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
    OldVsn :: term() | {down, term()},
    StateName :: atom(),
    StateData :: map(),
    Extra :: term(),
    NextStateName :: atom(),
    NewStateData :: map().
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