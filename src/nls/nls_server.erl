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
-export([start_link/1,
    is_valid_lang/1,
    response_content/4,
    get_nls_content/3,
    show_langs/2,
    read_nls_file/2,
    do_response_content/4,
    get_nls_map/1,
    merge_nls_map/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

-include("nls.hrl").

-type support_lang() :: zh | en.
-type lang_map() :: #{NlsKey :: atom() => NlsValue :: binary()}.
-type state() :: #{support_lang() => lang_map()}.
-type keys_map() :: #{Pos :: non_neg_integer() => FieldName :: atom()}.

-export_type([support_lang/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server by setting nls file name as server name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(NlsFilePath) ->
    {ok, Pid} |
    ignore |
    {error, Reason} when

    NlsFilePath :: file:filename_all(),
    Pid :: pid(),
    Reason :: term().
start_link(NlsFilePath) ->
    ServerNamesStr = filename:rootname(filename:basename(NlsFilePath)),
    [ServerNameStr | ExtraNlsFileNames] = string:tokens(ServerNamesStr, "."),

    ServerName = list_to_atom(ServerNameStr),
    io:format("nls server ~p starting~n", [ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, {ExtraNlsFileNames, NlsFilePath}, []).

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
    ContentList :: [{nls, NlsKey} | term()],
    NlsKey :: atom(),
    Lang :: support_lang(),
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
    ContentList :: [{nls, NlsKey} | term()],
    NlsKey :: atom(),
    Lang :: support_lang().
get_nls_content(ServerName, ContentList, Lang) ->
    gen_server:call(ServerName, {get_nls_content, ContentList, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether the current nls_server supports a langauge. Supported
%% languages maybe various from different nls_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_lang(Lang) -> boolean() when
    Lang :: support_lang().
is_valid_lang(Lang) ->
    gen_server:call(commands, {is_valid_lang, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Shows possible langauge abbreviations.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_langs(DispatcherPid, Lang) -> ok when
    DispatcherPid :: pid(),
    Lang :: support_lang().
show_langs(DispatcherPid, Lang) ->
    gen_server:cast(commands, {show_langs, DispatcherPid, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Reads nls values from csv file and return nls map.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_nls_file(NlsFileName, AccNlsMap) -> NlsMap when
    NlsFileName :: file:name_all(),
    AccNlsMap :: state(),
    NlsMap :: AccNlsMap.
read_nls_file(NlsFileName, AccNlsMap) ->
    {ok, NlsFile} = file:open(NlsFileName, [read]),
    {ok, NlsMap} = ecsv:process_csv_file_with(NlsFile, fun read_line/2, {0, AccNlsMap}),
    ok = file:close(NlsFile),
    NlsMap.

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for response_content/4.
%% @see response_content/4.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_response_content(Lang, State, ContentList, DispatcherPid) -> ok when
    Lang :: support_lang(),
    State :: state(),
    ContentList :: [term()],
    DispatcherPid :: pid().
do_response_content(Lang, State, ContentList, DispatcherPid) ->
    LangMap = maps:get(Lang, State),
    ReturnContent = fill_in_nls(ContentList, LangMap, []),
    command_dispatcher:return_content(DispatcherPid, ReturnContent).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the current nls map which is the target nls server state.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_nls_map(NlsServerName) -> NlsMap when
    NlsServerName :: atom(),
    NlsMap :: state().
get_nls_map(NlsServerName) ->
    gen_server:call(NlsServerName, get_nls_map).

%%--------------------------------------------------------------------
%% @doc
%% Merge two nls maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_nls_map(NlsMap1, NlsMap2) -> NlsMap when
    NlsMap1 :: state(),
    NlsMap2 :: NlsMap1,
    NlsMap :: NlsMap1.
merge_nls_map(NlsMap1, NlsMap2) ->
    Langs = maps:keys(NlsMap1),
    merge_nls_map(Langs, NlsMap1, NlsMap2, #{}).

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
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: {ExtraNlsFileNames, NlsFilePath},
    ExtraNlsFileNames :: [string()],
    NlsFilePath :: file:filename_all(),
    State :: state(),
    Reason :: term().
init({ExtraNlsFileNames, NlsFilePath}) ->
    CommonNlsFilePath = filename:append(?NLS_PATH, ?COMMON_NLS),
    CommonNlsMap = nls_server:read_nls_file(CommonNlsFilePath, #{}),
    NlsMap = read_nls_file(NlsFilePath, CommonNlsMap),
    FinalNlsMap = lists:foldl(fun load_nls_file/2, NlsMap, ExtraNlsFileNames),
    {ok, FinalNlsMap}.

%%--------------------------------------------------------------------
%% @doc
%% Loads nls file. This function is called by lists:foldl/3 or lists:foldr/3.
%% @see lists:foldl/3, lists:foldr/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_nls_file(NlsFileName, AccNlsMap) -> NlsMap when
    NlsFileName :: file:filename_all(),
    AccNlsMap :: state(),
    NlsMap :: AccNlsMap.
load_nls_file(NlsFileName, AccNlsMap) ->
    read_nls_file(filename:join(?NLS_PATH, NlsFileName) ++ ?NLS_EXTENSION, AccNlsMap).

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
    {is_valid_lang, Lang} |
    {get_nls_content, ConentList, Lang},

    NlsKey :: atom(),
    Lang :: support_lang(),
    ConentList :: [term()],
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: state(),
    NewState :: State,
    Reason :: term().
handle_call({get, NlsKey, Lang}, _From, State) ->
    NlsValue = maps:get(NlsKey, maps:get(Lang, State)),
    {reply, NlsValue, State};
handle_call({is_valid_lang, Lang}, _From, State) ->
    {reply, maps:is_key(Lang, State), State};
handle_call({get_nls_content, ContentList, Lang}, _From, State) ->
    LangMap = maps:get(Lang, State),
    ReturnContent = fill_in_nls(ContentList, LangMap, []),
    {reply, ReturnContent, State};
handle_call(get_nls_map, _From, State) ->
    {reply, State, State}.

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
    Lang :: support_lang(),
    DispatcherPid :: pid(),
    State :: state(),
    NewState :: State,
    Reason :: term().
handle_cast({response_content, ContentList, Lang, DispatcherPid}, State) ->
    do_response_content(Lang, State, ContentList, DispatcherPid),
    {noreply, State};
handle_cast({show_langs, DispatcherPid, Lang}, State) ->
    LangsNls = lists:reverse([[atom_to_binary(LangName, utf8), <<"\n">>] || LangName <- maps:keys(State)]),
    do_response_content(Lang, State, lists:flatten([{nls, possible_lang}, <<"\n">>, LangsNls]), DispatcherPid),
    {noreply, State}.

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
    State :: state(),
    NewState :: State,
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
    State :: state().
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
    State :: state(),
    Extra :: term(),
    NewState :: State,
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
%% Reads a line from csv file. Each line contains an nls key with its
%% supported language contents.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_line(NewLineData, State) -> FinalValuesMap when
    NewLineData :: {newline, NewLine} | {eof},
    NewLine :: [term()],
    State :: {Counter, KeysMap, ValuesMap},
    Counter :: non_neg_integer(),
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
    NewLine :: [string()],
    KeysMap :: keys_map(),
    ValuesMap :: lang_map(),
    Pos :: non_neg_integer(),
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
                               _ ->
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
    NewLine :: [term()],
    KeysMap :: keys_map(),
    ValuesMap :: lang_map(),
    Pos :: non_neg_integer(),
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
-spec fill_in_nls(ListIn, LangMap, ListOut) -> [binary()] when
    ListIn :: [{nls, NlsKey} | term()],
    NlsKey :: atom(),
    LangMap :: lang_map(),
    ListOut :: [binary()].
fill_in_nls([], _, ListOut) ->
    lists:reverse(ListOut);
fill_in_nls([{nls, NlsKey} | Tail], LangMap, ListOut) ->
    fill_in_nls(Tail, LangMap, [maps:get(NlsKey, LangMap) | ListOut]);
fill_in_nls([NonNlsKey | Tail], LangMap, ListOut) ->
    fill_in_nls(Tail, LangMap, [NonNlsKey | ListOut]).

%%--------------------------------------------------------------------
%% @doc
%% See parent function merge_nls_map/2.
%% @see merge_nls_map/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_nls_map(Langs, NlsMap1, NlsMap2, AccNlsMap) -> NlsMap when
    Langs :: [nls_server:support_lang()],
    NlsMap1 :: nls_server:state(),
    NlsMap2 :: NlsMap1,
    AccNlsMap :: NlsMap1,
    NlsMap :: NlsMap1.
merge_nls_map([CurLang | Tail], NlsMap1, NlsMap2, AccNlsMap) ->
    CurLangMap = maps:merge(maps:get(CurLang, NlsMap1, #{}), maps:get(CurLang, NlsMap2, #{})),
    merge_nls_map(Tail, NlsMap1, NlsMap2, AccNlsMap#{CurLang => CurLangMap});
merge_nls_map([], _, _, AccNlsMap) ->
    AccNlsMap.