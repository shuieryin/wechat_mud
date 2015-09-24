%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2015 12:22 PM
%%%-------------------------------------------------------------------
-module(nls_server).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([start_link/1,
    read_line/2,
    is_valid_lang/2,
    response_content/4,
    get_nls_content/3,
    show_langs/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-include("nls.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(ServerName) ->
    {ok, Pid} |
    ignore |
    {error, Reason} when

    ServerName :: atom(),
    Pid :: pid(),
    Reason :: term().
start_link(NlsFileName) ->
    ServerName = list_to_atom(filename:rootname(filename:basename(NlsFileName))),
    gen_server:start_link({local, ServerName}, ?MODULE, [NlsFileName], []).

%%--------------------------------------------------------------------
%% @doc
%% Given "ContentList" contains items {nls, NlsKey} with direct return
%% content values, the function is to replace {nls, NlsKey} with the actual
%% nls content, and then immediately return the result to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec response_content(ServerName, ContentList, Lang, DispatcherPid) -> ok when
    ServerName :: atom(),
    ContentList :: [term()],
    Lang :: atom(),
    DispatcherPid :: pid().
response_content(ServerName, ContentList, Lang, DispatcherPid) ->
    gen_server:cast(ServerName, {response_content, ContentList, Lang, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Given "ContentList" contains items {nls, NlsKey} with direct return
%% content values, the function is to replace {nls, NlsKey} with the actual
%% nls content.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_nls_content(ServerName, ContentList, Lang) -> [term()] when
    ServerName :: atom(),
    ContentList :: [term()],
    Lang :: atom().
get_nls_content(ServerName, ContentList, Lang) ->
    gen_server:call(ServerName, {get_nls_content, ContentList, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% check has language
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_lang(ServerName, Lang) -> boolean() when
    ServerName :: atom(),
    Lang :: atom().
is_valid_lang(ServerName, Lang) ->
    gen_server:call(ServerName, {is_valid_lang, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% show possible langauges
%%
%% @end
%%--------------------------------------------------------------------
-spec show_langs(DispatcherPid, Lang) -> ok when
    DispatcherPid :: pid(),
    Lang :: atom().
show_langs(DispatcherPid, Lang) ->
    gen_server:cast(lang, {show_langs, DispatcherPid, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Read line from csv file
%%
%% @end
%%--------------------------------------------------------------------
-spec read_line(NewLineData, State) -> FinalValuesMap when
    NewLineData :: {newline, NewLine} | {eof},
    NewLine :: [term()],
    State :: {Counter, KeysMap, ValuesMap},
    Counter :: non_neg_integer(),
    KeysMap :: map(),
    ValuesMap :: map(),
    FinalValuesMap :: map().
read_line({newline, NewLine}, {Counter, KeysMap, ValuesMap}) ->
    {Counter + 1, KeysMap, gen_valuesmap(NewLine, KeysMap, ValuesMap, 0)};
read_line({newline, NewLine}, {0, ValuesMap}) ->
    {KeysMap, NewValuesMap} = gen_keysmap(NewLine, #{}, 0, ValuesMap),
    {1, KeysMap, NewValuesMap};
read_line({eof}, {_, _, FinalValuesMap}) ->
    FinalValuesMap.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: [atom()],
    State :: map(),
    Reason :: term().
init([NlsFileName]) ->
    {ok, File} = file:open(NlsFileName, [read]),
    {ok, ValuesMap} = ecsv:process_csv_file_with(File, fun read_line/2, {0, #{}}),
    ok = file:close(File),

    {ok, CommonFile} = file:open(filename:append(?NLS_PATH, ?COMMON_NLS), [read]),
    {ok, FinalValuesMap} = ecsv:process_csv_file_with(CommonFile, fun read_line/2, {0, ValuesMap}),
    ok = file:close(CommonFile),

    error_logger:info_msg("FinalValuesMap:~p~n", [FinalValuesMap]),
    {ok, FinalValuesMap}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState} |
    {reply, Reply, NewState, timeout() | hibernate} |
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, Reply, NewState} |
    {stop, Reason, NewState} when

    Request :: {get, NlsKey, Lang} | {is_valid_lang, Lang} | {get_nls_content, ConentList, Lang},
    NlsKey :: atom(),
    Lang :: atom(),
    ConentList :: [term()],
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({get, NlsKey, Lang}, _From, State) ->
    NlsValue = maps:get(NlsKey, maps:get(Lang, State)),
    {reply, NlsValue, State};
handle_call({is_valid_lang, Lang}, _From, State) ->
    {reply, maps:is_key(Lang, State), State};
handle_call({get_nls_content, ContentList, Lang}, _From, State) ->
    LangMap = maps:get(Lang, State),
    ReturnContent = fill_in_nls(ContentList, LangMap, []),
    {reply, ReturnContent, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Request :: {response_content, ContentList, Lang, DispatcherPid} | {show_langs, DispatcherPid, Lang},
    ContentList :: [term()],
    Lang :: atom(),
    DispatcherPid :: pid(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_cast({response_content, ContentList, Lang, DispatcherPid}, State) ->
    do_response_content(Lang, State, ContentList, DispatcherPid),
    {noreply, State};
handle_cast({show_langs, DispatcherPid, Lang}, State) ->
    LangsNls = lists:reverse([[atom_to_binary(LangName, utf8), <<"\n">>] || LangName <- maps:keys(State)]),
    do_response_content(Lang, State, lists:flatten([{nls, possible_lang}, <<"\n">>, LangsNls]), DispatcherPid),
    {noreply, State}.

-spec fill_in_nls(ListIn, LangMap, ListOut) -> [binary()] when
    ListIn :: [term()],
    LangMap :: map(),
    ListOut :: [binary()].
fill_in_nls([], _, ListOut) ->
    lists:reverse(ListOut);
fill_in_nls([{nls, NlsKey} | Tail], LangMap, ListOut) ->
    fill_in_nls(Tail, LangMap, [maps:get(NlsKey, LangMap) | ListOut]);
fill_in_nls([NonNlsKey | Tail], LangMap, ListOut) ->
    fill_in_nls(Tail, LangMap, [NonNlsKey | ListOut]).

-spec do_response_content(Lang, State, ContentList, DispatcherPid) -> ok when
    Lang :: atom(),
    State :: map(),
    ContentList :: [term()],
    DispatcherPid :: pid().
do_response_content(Lang, State, ContentList, DispatcherPid) ->
    LangMap = maps:get(Lang, State),
    ReturnContent = fill_in_nls(ContentList, LangMap, []),
    command_dispatcher:return_content(DispatcherPid, ReturnContent).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info | term(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: timeout(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> term() when
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: map().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
    {ok, NewState} |
    {error, Reason} when

    OldVsn :: term() | {down, term()},
    State :: map(),
    Extra :: term(),
    NewState :: map(),
    Reason :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% generate keys map from first line
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_keysmap(NewLine, KeysMap, Pos, ValuesMap) -> {KeysMap, FinalKeysMap} when
    NewLine :: [string()],
    KeysMap :: map(),
    ValuesMap :: map(),
    Pos :: non_neg_integer(),
    FinalKeysMap :: map().
gen_keysmap([], KeysMap, _, ValuesMap) ->
    {KeysMap, ValuesMap};
gen_keysmap([RawKey | Tail], KeysMap, Pos, ValuesMap) ->
    Key = list_to_atom(RawKey),
    NewValuesMap = case Pos of
                       0 ->
                           ValuesMap;
                       _ ->
                           case maps:is_key(Key, ValuesMap) of
                               false ->
                                   ValuesMap#{Key => #{}};
                               _ ->
                                   ValuesMap
                           end
                   end,
    gen_keysmap(Tail, KeysMap#{Pos => Key}, Pos + 1, NewValuesMap).

%%--------------------------------------------------------------------
%% @doc
%% generate values map from rest of lines
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_valuesmap(NewLine, KeysMap, ValuesMap, Pos) -> FinalValuesMap when
    NewLine :: [term()],
    KeysMap :: map(),
    ValuesMap :: map(),
    Pos :: non_neg_integer(),
    FinalValuesMap :: map().
gen_valuesmap([], _, ValueMap, _) ->
    ValueMap;
gen_valuesmap([[] | Tail], KeysMap, ValuesMap, Pos) ->
    gen_valuesmap(Tail, KeysMap, ValuesMap, Pos + 1);
gen_valuesmap([Value | Tail], KeysMap, ValuesMap, Pos) ->
    Key = maps:get(Pos, KeysMap),
    {NewKeysMap, NewValueMap} = case Key of
                                    id ->
                                        Id = list_to_atom(Value),
                                        {KeysMap#{cur_id => Id}, ValuesMap};
                                    Lang ->
                                        Id = maps:get(cur_id, KeysMap),
                                        LangMap = maps:get(Lang, ValuesMap),
                                        {KeysMap, ValuesMap#{Lang := LangMap#{Id => re:replace(Value, "~n", "\n", [global, {return, binary}])}}}
                                end,
    gen_valuesmap(Tail, NewKeysMap, NewValueMap, Pos + 1).
