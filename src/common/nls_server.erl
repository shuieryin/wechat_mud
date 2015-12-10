%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Nls gen_server. This gen_server acts as template server and is
%%% initialized per csv file under priv/nls by nls_sup.erl
%%% which the number of initialized nls_server (server name is set
%%% as nls file name) equals to the number of nls files. Its job is
%%% to hold the nls content and generate formatted response text to
%%% player.
%%%
%%% @end
%%% Created : 12. Sep 2015 12:22 PM
%%%-------------------------------------------------------------------
-module(nls_server).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    is_valid_lang/1,
    response_content/3,
    get_nls_content/2,
    show_langs/2,
    do_response_content/3,
    get_lang_map/1,
    start/0,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).

%% Nls files root path
-define(NLS_PATH, filename:join(code:priv_dir(wechat_mud), "nls_server")).

%% Common nls file name
-define(COMMON_NLS, "common.csv").

%% Unique nls file extension
-define(NLS_EXTENSION, ".csv").

-type support_lang() :: zh | en.
-type key() :: atom(). % generic atom
-type value() :: binary().
-type field_name() :: atom(). % generic atom
-type lang_map() :: #{key() => value()}.
-type key_pos() :: non_neg_integer(). % generic integer
-type keys_map() :: #{key_pos() => field_name()}.
-type nls_object() :: {nls, key()} | value().
-type nls_map() :: #{support_lang() => lang_map()}.

-record(state, {
    nls_map :: nls_map(),
    valid_langs :: [binary()]
}).

-export_type([support_lang/0,
    nls_object/0,
    lang_map/0,
    key/0,
    value/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server by setting nls file name as server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts server by setting module name as server name without link.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> gen:start_ret().
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop server.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Given "NlsObjectList" contains items {nls, NlsKey} with direct return
%% content values, the function is to replace {nls, NlsKey} with the actual
%% nls content, and then immediately return the result to user.
%%
%% @end
%%--------------------------------------------------------------------
-spec response_content(NlsObjectList, Lang, DispatcherPid) -> ok when
    NlsObjectList :: [nls_server:nls_object()],
    Lang :: support_lang(),
    DispatcherPid :: pid().
response_content(NlsObjectList, Lang, DispatcherPid) ->
    gen_server:cast(?SERVER, {response_content, NlsObjectList, Lang, DispatcherPid}).

%%--------------------------------------------------------------------
%% @doc
%% Given "NlsObjectList" contains items {nls, NlsKey} with direct return
%% content values, the function is to replace {nls, NlsKey} with the actual
%% nls content.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_nls_content(NlsObjectList, Lang) -> ContentList when
    NlsObjectList :: [nls_server:nls_object()],
    Lang :: support_lang(),
    ContentList :: [value()].
get_nls_content(NlsObjectList, Lang) ->
    gen_server:call(?SERVER, {get_nls_content, NlsObjectList, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether the current nls_server supports a langauge. Supported
%% languages maybe various from different nls_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_lang(Lang) -> boolean() when
    Lang :: support_lang() | binary().
is_valid_lang(Lang) ->
    gen_server:call(?SERVER, {is_valid_lang, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Shows possible language abbreviations.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_langs(DispatcherPid, Lang) -> ok when
    DispatcherPid :: pid(),
    Lang :: support_lang().
show_langs(DispatcherPid, Lang) ->
    gen_server:cast(?SERVER, {show_langs, DispatcherPid, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for response_content/3.
%% @see response_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_response_content(Lang, State, NlsObjectList, DispatcherPid) -> ok when
    Lang :: support_lang(),
    State :: #state{},
    NlsObjectList :: [nls_server:nls_object()],
    DispatcherPid :: pid().
do_response_content(Lang, #state{nls_map = NlsMap}, NlsObjectList, DispatcherPid) ->
    LangMap = maps:get(Lang, NlsMap),
    do_response_content(LangMap, NlsObjectList, DispatcherPid).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for response_content/3.
%% @see response_content/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_response_content(LangMap, NlsObjectList, DispatcherPid) -> ok when
    LangMap :: lang_map(),
    NlsObjectList :: [nls_server:nls_object()],
    DispatcherPid :: pid().
do_response_content(LangMap, NlsObjectList, DispatcherPid) ->
    ReturnContent = fill_in_nls(NlsObjectList, LangMap, []),
    command_dispatcher:return_content(DispatcherPid, ReturnContent).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the current nls map which is the target nls server state.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_lang_map(Lang) -> LangMap when
    Lang :: support_lang(),
    LangMap :: lang_map().
get_lang_map(Lang) ->
    gen_server:call(?SERVER, {get_lang_map, Lang}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the nls_server by parsing nls keys and values from csv
%% file to map and put it in state.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    io:format("nls server starting..."),

    NlsPath = filename:join(code:priv_dir(wechat_mud), "nls_server"),
    {ok, FileNameList} = file:list_dir(NlsPath),

    CommonNlsFilePath = filename:append(NlsPath, ?COMMON_NLS),
    CommonNlsMap = read_nls_file(CommonNlsFilePath, #{}),
    NlsMap = lists:foldl(fun load_nls_file/2, CommonNlsMap, FileNameList),

    io:format("started~n"),
    {ok, #state{nls_map = NlsMap, valid_langs = [atom_to_binary(ValidLang, utf8) || ValidLang <- maps:keys(NlsMap)]}}.

%%--------------------------------------------------------------------
%% @doc
%% Loads nls file. This function is called by lists:foldl/3 or lists:foldr/3.
%% @see lists:foldl/3.
%% @see lists:foldr/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_nls_file(NlsFileName, AccNlsMap) -> NlsMap when
    NlsFileName :: file:filename_all(),
    AccNlsMap :: nls_map(),
    NlsMap :: AccNlsMap.
load_nls_file(NlsFileName, AccNlsMap) ->
    read_nls_file(filename:join(?NLS_PATH, NlsFileName), AccNlsMap).

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

    Request ::
    {get, NlsKey, Lang} |
    {is_valid_lang, TargetLang} |
    {get_nls_content, NlsObjectList, Lang} |
    {get_lang_map, Lang} |
    stop,

    Reply :: State | ContentList | IsValidLang | NlsValue,

    NlsKey :: erlang:registered_name(),
    NlsValue :: value(),
    Lang :: support_lang(),
    TargetLang :: Lang | binary(),
    NlsObjectList :: [nls_server:nls_object()],
    ContentList :: [NlsValue],
    IsValidLang :: boolean(),

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call({get, NlsKey, Lang}, _From, #state{nls_map = NlsMap} = State) ->
    NlsValue = maps:get(NlsKey, maps:get(Lang, NlsMap)),
    {reply, NlsValue, State};
handle_call({is_valid_lang, TargetLang}, _From, #state{nls_map = NlsMap, valid_langs = ValidLangs} = State) ->
    Result = case cm:type_of(TargetLang) of
                 binary ->
                     lists:member(TargetLang, ValidLangs);
                 atom ->
                     maps:is_key(TargetLang, NlsMap);
                 _ ->
                     false
             end,
    {reply, Result, State};
handle_call({get_nls_content, NlsObjectList, Lang}, _From, #state{nls_map = NlsMap} = State) ->
    LangMap = maps:get(Lang, NlsMap),
    ReturnContent = fill_in_nls(NlsObjectList, LangMap, []),
    {reply, ReturnContent, State};
handle_call({get_lang_map, Lang}, _From, #state{nls_map = NlsMap} = State) ->
    {reply, maps:get(Lang, NlsMap), State}.

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

    Request :: {response_content, NlsObjectList, Lang, DispatcherPid} | {show_langs, DispatcherPid, Lang},
    NlsObjectList :: [nls_server:nls_object()],
    Lang :: support_lang(),
    DispatcherPid :: pid(),
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast({response_content, NlsObjectList, Lang, DispatcherPid}, State) ->
    do_response_content(Lang, State, NlsObjectList, DispatcherPid),
    {noreply, State};
handle_cast({show_langs, DispatcherPid, Lang}, #state{nls_map = NlsMap} = State) ->
    LangsNls = lists:reverse([[atom_to_binary(LangName, utf8), <<"\n">>] || LangName <- maps:keys(NlsMap)]),
    do_response_content(Lang, State, lists:flatten([{nls, possible_lang}, <<"\n">>, LangsNls]), DispatcherPid),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

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
-spec handle_info(Info | timeout(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: term(), % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
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
-spec terminate(Reason, State) -> ok when
    Reason :: (normal | shutdown | {shutdown, term()} | term()), % generic term
    State :: #state{}.
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

    OldVsn :: term() | {down, term()}, % generic term
    State :: #state{},
    Extra :: term(), % generic term
    NewState :: State,
    Reason :: term(). % generic term
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
    PDict :: [{Key :: term(), Value :: term()}], % generic term
    State :: #state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads a line from csv file. Each line contains an nls key with its
%% supported language contents.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_line(NewLineData, ReadLineState) -> FinalValuesMap when
    NewLineData :: {newline, NewLine} | {eof},
    NewLine :: [csv_to_object:csv_line()],
    ReadLineState :: {Counter, KeysMap, ValuesMap} | {0, ValuesMap},
    Counter :: key_pos(),
    KeysMap :: keys_map(),
    ValuesMap :: lang_map(),
    FinalValuesMap :: ValuesMap.
read_line({newline, NewLine}, {Counter, KeysMap, ValuesMap}) ->
    {Counter + 1, KeysMap, gen_valuesmap(NewLine, KeysMap, ValuesMap, 0)};
read_line({newline, NewLine}, {0, ValuesMap}) ->
    {KeysMap, NewValuesMap} = gen_keysmap(NewLine, #{}, 0, ValuesMap),
    {1, KeysMap, NewValuesMap};
read_line({eof}, {_, _, FinalValuesMap}) ->
    FinalValuesMap.

%%--------------------------------------------------------------------
%% @doc
%% Generate keys map from first line from csv file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_keysmap(NewLine, KeysMap, Pos, ValuesMap) -> {KeysMap, FinalKeysMap} when
    NewLine :: [csv_to_object:csv_line()],
    KeysMap :: keys_map(),
    ValuesMap :: lang_map(),
    Pos :: key_pos(),
    FinalKeysMap :: KeysMap.
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
                               true ->
                                   ValuesMap
                           end
                   end,
    gen_keysmap(Tail, KeysMap#{Pos => Key}, Pos + 1, NewValuesMap).

%%--------------------------------------------------------------------
%% @doc
%% Generates values map from rest of lines from csv file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_valuesmap(NewLine, KeysMap, ValuesMap, Pos) -> FinalValuesMap when
    NewLine :: [csv_to_object:csv_line()],
    KeysMap :: keys_map(),
    ValuesMap :: lang_map(),
    Pos :: key_pos(),
    FinalValuesMap :: ValuesMap.
gen_valuesmap([], _, ValueMap, _) ->
    ValueMap;
gen_valuesmap([[] | Tail], KeysMap, ValuesMap, Pos) ->
    gen_valuesmap(Tail, KeysMap, ValuesMap, Pos + 1);
gen_valuesmap([Value | Tail], KeysMap, ValuesMap, Pos) ->
    Key = maps:get(Pos, KeysMap),
    {NewKeysMap, NewValueMap} =
        case Key of
            id ->
                Id = list_to_atom(Value),
                {KeysMap#{cur_id => Id}, ValuesMap};
            Lang ->
                Id = maps:get(cur_id, KeysMap),
                LangMap = maps:get(Lang, ValuesMap),
                FinalValue = re:replace(Value, "~n", "\n", [global, {return, binary}]),
                {KeysMap, ValuesMap#{Lang := LangMap#{Id => FinalValue}}}
        end,
    gen_valuesmap(Tail, NewKeysMap, NewValueMap, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Replaces nls key with actual nls content by language.
%%
%% @end
%%--------------------------------------------------------------------
-spec fill_in_nls(NlsObjectList, LangMap, AccContentList) -> ContentList when
    NlsObjectList :: [nls_server:nls_object()],
    LangMap :: lang_map(),
    AccContentList :: [value()],
    ContentList :: AccContentList.
fill_in_nls([], _, AccContentList) ->
    lists:reverse(AccContentList);
fill_in_nls([{nls, NlsKey} | Tail], LangMap, AccContentList) ->
    fill_in_nls(Tail, LangMap, [maps:get(NlsKey, LangMap) | AccContentList]);
fill_in_nls([NonNlsKey | Tail], LangMap, AccContentList) ->
    fill_in_nls(Tail, LangMap, [NonNlsKey | AccContentList]).

%%--------------------------------------------------------------------
%% @doc
%% Reads nls values from csv file and return nls map.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_nls_file(NlsFileName, AccNlsMap) -> NlsMap when
    NlsFileName :: file:name_all(),
    AccNlsMap :: nls_map(),
    NlsMap :: AccNlsMap.
read_nls_file(NlsFileName, AccNlsMap) ->
    {ok, NlsFile} = file:open(NlsFileName, [read]),
    {ok, NlsMap} = ecsv:process_csv_file_with(NlsFile, fun read_line/2, {0, AccNlsMap}),
    ok = file:close(NlsFile),
    NlsMap.