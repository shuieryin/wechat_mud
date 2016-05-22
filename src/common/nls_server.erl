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
    lang_map/1,
    start/0,
    stop/0,
    fill_in_content/3,
    convert_target_nls/4,
    nls_file_name_map/0,
    get_nls_langs/1
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

%% Common nls file name
-define(COMMON_NLS, "common.csv").

%% Unique nls file extension
-define(NLS_EXTENSION, ".csv").

-type support_lang() :: zh | en.
-type key() :: atom(). % generic atom
-type value() :: term(). % generic term
-type field_name() :: atom(). % generic atom
-type lang_map() :: #{key() => value()}.
-type key_pos() :: non_neg_integer(). % generic integer
-type keys_map() :: #{key_pos() => field_name()}.
-type nls_replacements() :: [value() | nls_object()].
-type nls_object() :: {nls, key()} | {nls, key(), nls_replacements()} | {value(), nls_replacements()} | value().
-type nls_map() :: #{support_lang() => lang_map()}.
-type nls_file_name() :: atom(). % generic atom
-type nls_file_name_map() :: #{nls_file_name() => gb_sets:set(key())}.

-record(state, {
    nls_map :: nls_map(),
    nls_file_name_map :: nls_file_name_map(),
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
%% Given "NlsKeyList" contains items [NlsKey] to retrieve [NlsValue] of all langugages.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_nls_langs(NlsKeyList) -> NlsValuesList when
    NlsKeyList :: [key()],
    NlsValueList :: [value()],
    NlsValuesList :: [NlsValueList].
get_nls_langs(NlsKeyList) ->
    gen_server:call(?SERVER, {get_nls_langs, NlsKeyList}).

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
do_response_content(
    Lang,
    #state{
        nls_map = NlsMap
    },
    NlsObjectList,
    DispatcherPid
) ->
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
-spec lang_map(Lang) -> LangMap when
    Lang :: support_lang(),
    LangMap :: lang_map().
lang_map(Lang) ->
    gen_server:call(?SERVER, {lang_map, Lang}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the current nls file name map.
%%
%% @end
%%--------------------------------------------------------------------
-spec nls_file_name_map() -> nls_file_name_map().
nls_file_name_map() ->
    gen_server:call(?SERVER, nls_file_name_map).

%%--------------------------------------------------------------------
%% @doc
%% Replace "${any()}" in binary with replacements in order:
%%      "ab${content_abbrv}ef" replace ["cd"] =
%%                  "abcdef";
%%      "ab${}ef" replace ["cd"] =
%%                  "abcdef";
%%      "ab${content_abbrv}ef" replace ["cd", "will_be_ignore"] =
%%                  "abcdef";
%%      "ab${content_abbrv}ef" replace [] =
%%                  "ab${content_abbrv}ef";
%%
%% @end
%%--------------------------------------------------------------------
-spec fill_in_content(SrcContent, Replacements, AccContent) -> FinalContent when
    SrcContent :: value(),
    Replacements :: [value()],
    AccContent :: SrcContent,
    FinalContent :: AccContent.
fill_in_content(<<"${}", Rest/binary>>, [RawReplacement | Replacements], AccContent) ->
    Replacement = cm:to_binary(RawReplacement),
    fill_in_content(Rest, Replacements, <<AccContent/binary, Replacement/binary>>);
fill_in_content(<<"${", _IgnoreOneByte, Rest/binary>>, Replacements, AccContent) ->
    fill_in_content(<<"${", Rest/binary>>, Replacements, AccContent);
fill_in_content(<<Byte, Rest/binary>>, Replacements, AccContent) ->
    fill_in_content(Rest, Replacements, <<AccContent/binary, Byte>>);
fill_in_content(<<>>, _Replacements, FinalContent) ->
    FinalContent.

%%--------------------------------------------------------------------
%% @doc
%% Convert target nls keys to value.
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_target_nls(SrcNlsObjectList, LangMap, TargetNlsSet, AccNlsObjectList) -> NlsObjectList when
    SrcNlsObjectList :: [nls_object()],
    LangMap :: lang_map(),
    TargetNlsSet :: gb_sets:set(atom()), % generic atom
    AccNlsObjectList :: SrcNlsObjectList,
    NlsObjectList :: SrcNlsObjectList.
convert_target_nls([{nls, NlsKey} | RestMessage], LangMap, TargetNlsSet, AccNlsObjectList) ->
    NlsObject = case gb_sets:is_member(NlsKey, TargetNlsSet) of
                    true ->
                        maps:get(NlsKey, LangMap);
                    false ->
                        {nls, NlsKey}
                end,
    convert_target_nls(RestMessage, LangMap, TargetNlsSet, [NlsObject | AccNlsObjectList]);
convert_target_nls([{nls, NlsKey, Replacements} | RestMessage], LangMap, TargetNlsSet, AccNlsObjectList) ->
    ConvertedReplacements = convert_target_nls(Replacements, LangMap, TargetNlsSet, []),
    NlsObject = case gb_sets:is_member(NlsKey, TargetNlsSet) of
                    true ->
                        {maps:get(NlsKey, LangMap), ConvertedReplacements};
                    false ->
                        {nls, NlsKey, ConvertedReplacements}
                end,
    convert_target_nls(RestMessage, LangMap, TargetNlsSet, [NlsObject | AccNlsObjectList]);
convert_target_nls([{ConvertedValue, Replacements} | RestMessage], LangMap, TargetNlsSet, AccNlsObjectList) ->
    ConvertedReplacements = convert_target_nls(Replacements, LangMap, TargetNlsSet, []),
    convert_target_nls(RestMessage, LangMap, TargetNlsSet, [{ConvertedValue, ConvertedReplacements} | AccNlsObjectList]);
convert_target_nls([Other | RestMessage], LangMap, TargetNlsSet, AccNlsObjectList) ->
    ConvertedNlsObject =
        if
            is_list(Other) ->
                convert_target_nls(Other, LangMap, TargetNlsSet, []);
            true ->
                Other
        end,
    convert_target_nls(RestMessage, LangMap, TargetNlsSet, [ConvertedNlsObject | AccNlsObjectList]);
convert_target_nls([], _LangMap, _TargetNlsSet, NlsObjectList) ->
    lists:reverse(NlsObjectList).

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

    NlsPath = filename:join(code:priv_dir(cm:app_name()), ?MODULE_STRING),
    {ok, FileNameList} = file:list_dir(NlsPath),

    CommonNlsFilePath = filename:append(NlsPath, ?COMMON_NLS),
    FilePathList = [filename:join(NlsPath, NlsFileName) || NlsFileName <- FileNameList],
    {NlsMap, _DiffNlsMap, NlsFileNameMap} = lists:foldl(fun read_nls_file/2, {#{}, #{}, #{}}, [CommonNlsFilePath | FilePathList]),

    io:format("started~n"),
    {
        ok,
        #state{
            nls_map = NlsMap,
            nls_file_name_map = NlsFileNameMap,
            valid_langs = [atom_to_binary(ValidLang, utf8) || ValidLang <- maps:keys(NlsMap)]
        }
    }.

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
    {get_nls_langs, NlsKeyList} |
    {lang_map, Lang} |
    stop,

    Reply :: State | ContentList | IsValidLang | NlsValue | NlsValuesList,

    NlsKey :: erlang:registered_name(),
    NlsValue :: value(),
    Lang :: support_lang(),
    TargetLang :: Lang | binary(),
    NlsObjectList :: [nls_server:nls_object()],
    ContentList :: [NlsValue],
    IsValidLang :: boolean(),
    NlsKeyList :: [key()],
    NlsValueList :: [NlsValue],
    NlsValuesList :: [NlsValueList],

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(
    {get, NlsKey, Lang},
    _From,
    #state{
        nls_map = NlsMap
    } = State
) ->
    NlsValue = maps:get(NlsKey, maps:get(Lang, NlsMap)),
    {reply, NlsValue, State};
handle_call(
    {is_valid_lang, TargetLang},
    _From,
    #state{
        nls_map = NlsMap,
        valid_langs = ValidLangs
    } = State
) ->
    Result = case cm:type_of(TargetLang) of
                 binary ->
                     lists:member(TargetLang, ValidLangs);
                 atom ->
                     maps:is_key(TargetLang, NlsMap);
                 _OtherType ->
                     false
             end,
    {reply, Result, State};
handle_call(
    {get_nls_content, NlsObjectList, Lang},
    _From,
    #state{
        nls_map = NlsMap
    } = State
) ->
    LangMap = maps:get(Lang, NlsMap),
    ReturnContent = fill_in_nls(NlsObjectList, LangMap, []),
    {reply, ReturnContent, State};
handle_call(
    {get_nls_langs, NlsKeyList},
    _From,
    #state{
        nls_map = NlsMap
    } = State
) ->
    NlsValuesList = maps:fold(
        fun(_Lang, LangMap, AccLangsList) ->
            CurLangsList = lists:foldl(
                fun({nls, NlsKey}, AccCurLangsList) ->
                    CurNlsValue = maps:get(NlsKey, LangMap),
                    [CurNlsValue | AccCurLangsList]
                end, [], NlsKeyList),

            {UpdatedAccLangsList, []} =
                case AccLangsList of
                    [] ->
                        NewLangsList = lists:foldl(
                            fun(NlsValue, AccNewLangsList) ->
                                [[NlsValue] | AccNewLangsList]
                            end, [], CurLangsList),
                        {NewLangsList, []};
                    _HasNlsValues ->
                        lists:foldl(
                            fun(NlsValue, {AccAccLangsList, [AccLangs | OriAccLangsList]}) ->
                                {[[NlsValue | AccLangs] | AccAccLangsList], OriAccLangsList}
                            end, {[], AccLangsList}, CurLangsList)
                end,
            lists:reverse(UpdatedAccLangsList)
        end, [], NlsMap),
    {reply, NlsValuesList, State};
handle_call(
    {lang_map, Lang},
    _From,
    #state{
        nls_map = NlsMap
    } = State
) ->
    {reply, maps:get(Lang, NlsMap), State};
handle_call(nls_file_name_map, _From, #state{
    nls_file_name_map = NlsFileNameMap
} = State) ->
    {reply, NlsFileNameMap, State}.

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
handle_cast(
    {show_langs, DispatcherPid, Lang},
    #state{
        nls_map = NlsMap
    } = State
) ->
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
code_change(_OldVsn, #state{
    nls_map = OldNlsMap,
    nls_file_name_map = OldNlsFileNameMap
} = State, Extra) ->
    try
        case Extra of
            {_OldVer, _NewVsn, PrivChangedFiles} ->
                case csv_to_object:convert_priv_paths(PrivChangedFiles) of
                    no_change ->
                        {ok, State};
                    {ModifiedFilePaths, AddedFilePaths, DeletedFileNames} ->
                        {UpdatedOldNlsMap, UpdatedOldNlsFileNameMap, RemovedNlsSet} =
                            lists:foldl(
                                fun(DeletedFileName, {AccNlsMap, AccNlsFileNameMap, AccRemovedNlsSet}) ->
                                    case maps:get(DeletedFileName, OldNlsFileNameMap, undefined) of
                                        undefined ->
                                            {AccNlsMap, AccNlsFileNameMap, AccRemovedNlsSet};
                                        KeysSetToBeRemoved ->
                                            KeysToBeRemoved = gb_sets:to_list(KeysSetToBeRemoved),
                                            UpdatedAccNlsMap =
                                                maps:fold(
                                                    fun(Lang, LangMap, AccUpdatedAccNlsMap) ->
                                                        AccUpdatedAccNlsMap#{
                                                            Lang => maps:without(KeysToBeRemoved, LangMap)
                                                        }
                                                    end, #{}, AccNlsMap),

                                            UpdatedAccRemovedNlsSet =
                                                gb_sets:fold(
                                                    fun(KeyToBeRemoved, AccAccRemovedNlsSet) ->
                                                        gb_sets:add(KeyToBeRemoved, AccAccRemovedNlsSet)
                                                    end, AccRemovedNlsSet, KeysSetToBeRemoved
                                                ),

                                            {
                                                UpdatedAccNlsMap,
                                                maps:remove(DeletedFileName, AccNlsFileNameMap),
                                                UpdatedAccRemovedNlsSet
                                            }
                                    end
                                end, {OldNlsMap, OldNlsFileNameMap, gb_sets:new()}, DeletedFileNames),

                        ReloadFilePaths = AddedFilePaths ++ ModifiedFilePaths, % number of add files is usually less than modified files
                        {_NewChangedNlsMap, DiffNlsMap, NewChangedNlsFileNameMap} = lists:foldl(fun read_nls_file/2, {UpdatedOldNlsMap, #{}, #{}}, ReloadFilePaths),

                        NewNlsMap = maps:fold(
                            fun(Lang, NewDiffLangMap, AccNewNlsMap) ->
                                OldLangMap = maps:get(Lang, AccNewNlsMap),
                                AccNewNlsMap#{
                                    Lang := maps:merge(OldLangMap, NewDiffLangMap)
                                }
                            end, UpdatedOldNlsMap, DiffNlsMap
                        ),

                        NewNlsFileNameMap = maps:merge(UpdatedOldNlsFileNameMap, NewChangedNlsFileNameMap),

                        AddedFileNamesSet = gb_sets:from_list([list_to_atom(filename:rootname(filename:basename(AddedFileName))) || AddedFileName <- AddedFilePaths]),
                        UpdatedAddedFileNamesSet = maps:fold(
                            fun(ChangedFileName, _FileNameMap, AccAddedFileNamesSet) ->
                                case maps:is_key(ChangedFileName, UpdatedOldNlsFileNameMap) of
                                    false ->
                                        gb_sets:add(ChangedFileName, AccAddedFileNamesSet);
                                    true ->
                                        AccAddedFileNamesSet
                                end
                            end, AddedFileNamesSet, NewChangedNlsFileNameMap),

                        {UpdatedRemovedNlsSet, UpdatedNewNlsFileNameMap, UpdatedNewNlsMap} =
                            maps:fold(
                                fun(FileName, OldKeysSet, {AccUpdatedRemovedNlsSet, AccNewNlsFileNameMap, AccUpdatedNewNlsMap}) ->
                                    case maps:get(FileName, NewChangedNlsFileNameMap, undefined) of
                                        undefined ->
                                            {AccUpdatedRemovedNlsSet, AccNewNlsFileNameMap, AccUpdatedNewNlsMap};
                                        NewKeysSet ->
                                            {UpdatedAccUpdatedRemovedNlsSet, UpdatedAccNewKeysSet, UpdatedAccUpdatedNewNlsMap} =
                                                gb_sets:fold(
                                                    fun(OldKey, {AccAccUpdatedRemovedNlsSet, AccNewKeysSet, AccAccUpdatedNewNlsMap}) ->
                                                        case gb_sets:is_member(OldKey, NewKeysSet) of
                                                            false ->
                                                                {
                                                                    gb_sets:add(OldKey, AccAccUpdatedRemovedNlsSet),
                                                                    gb_sets:del_element(OldKey, AccNewKeysSet),
                                                                    maps:fold(
                                                                        fun(AccLang, AccNewLangMap, AccAccAccUpdatedNewNlsMap) ->
                                                                            AccAccAccUpdatedNewNlsMap#{
                                                                                AccLang := maps:remove(OldKey, AccNewLangMap)
                                                                            }
                                                                        end, AccAccUpdatedNewNlsMap, AccAccUpdatedNewNlsMap
                                                                    )
                                                                };
                                                            true ->
                                                                {AccAccUpdatedRemovedNlsSet, AccNewKeysSet, AccAccUpdatedNewNlsMap}
                                                        end
                                                    end, {AccUpdatedRemovedNlsSet, NewKeysSet, AccUpdatedNewNlsMap}, OldKeysSet
                                                ),
                                            {
                                                UpdatedAccUpdatedRemovedNlsSet,
                                                AccNewNlsFileNameMap#{
                                                    FileName => UpdatedAccNewKeysSet
                                                },
                                                UpdatedAccUpdatedNewNlsMap
                                            }
                                    end
                                end, {RemovedNlsSet, NewNlsFileNameMap, NewNlsMap}, UpdatedOldNlsFileNameMap),

                        error_logger:info_msg("~p~n============updated nls~n~tp~n============removed nls~n~p~n============added nls file~n~p~n============removed nls file~n~p~n", [?MODULE_STRING, DiffNlsMap, gb_sets:to_list(UpdatedRemovedNlsSet), gb_sets:to_list(UpdatedAddedFileNamesSet), DeletedFileNames]),

                        ok = gb_sets:fold(
                            fun(PlayerUid, ok) ->
                                PlayerLang = player_fsm:get_lang(PlayerUid),
                                PlayerDiffLangMap = maps:get(PlayerLang, DiffNlsMap, #{}),
                                case gb_sets:is_empty(UpdatedRemovedNlsSet) of
                                    true ->
                                        IsPlayerDiffLangMapEmpty = maps:size(PlayerDiffLangMap) == 0,
                                        case IsPlayerDiffLangMapEmpty of
                                            true ->
                                                ok;
                                            false ->
                                                player_fsm:update_nls(PlayerUid, PlayerDiffLangMap, UpdatedRemovedNlsSet)
                                        end;
                                    false ->
                                        player_fsm:update_nls(PlayerUid, PlayerDiffLangMap, UpdatedRemovedNlsSet)
                                end
                            end, ok, login_server:logged_in_player_uids()),

                        {ok, State#state{
                            nls_map = UpdatedNewNlsMap,
                            nls_file_name_map = UpdatedNewNlsFileNameMap
                        }}
                end;
            _NoChange ->
                {ok, State}
        end
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
-spec read_line(NewLineData, ReadLineState) -> {ValuesMap, DiffValuesMap, KeysSet} when
    NewLineData :: {newline, NewLine} | {eof},
    NewLine :: [csv_to_object:csv_line()],
    ReadLineState :: {Counter, KeysMap, {AccValuesMap, AccDiffValuesMap, AccKeysSet}} | {0, AccValuesMap, AccDiffValuesMap},
    Counter :: key_pos(),
    KeysMap :: keys_map(),
    AccValuesMap :: lang_map(),
    AccDiffValuesMap :: AccValuesMap,
    AccKeysSet :: gb_sets:set(key()),
    ValuesMap :: AccValuesMap,
    DiffValuesMap :: AccValuesMap,
    KeysSet :: AccKeysSet.
read_line({newline, NewLine}, {Counter, KeysMap, {AccValuesMap, AccDiffValuesMap, AccKeysSet}}) ->
    {Counter + 1, KeysMap, gen_valuesmap(NewLine, KeysMap, AccValuesMap, AccDiffValuesMap, AccKeysSet, 0)};
read_line({newline, NewLine}, {0, AccValuesMap, AccDiffValuesMap}) ->
    {KeysMap, UpdatedAccValuesMap} = gen_keysmap(NewLine, #{}, 0, AccValuesMap),
    {1, KeysMap, {UpdatedAccValuesMap, AccDiffValuesMap, gb_sets:new()}};
read_line({eof}, {_Counter, _KeysMap, {ValuesMap, DiffValuesMap, KeysSet}}) ->
    {ValuesMap, DiffValuesMap, KeysSet}.

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
gen_keysmap([], KeysMap, _Pos, ValuesMap) ->
    {KeysMap, ValuesMap};
gen_keysmap([RawKey | Tail], KeysMap, Pos, ValuesMap) ->
    Key = list_to_atom(RawKey),
    NewValuesMap = case Pos of
                       0 ->
                           ValuesMap;
                       _Pos ->
                           case maps:is_key(Key, ValuesMap) of
                               false ->
                                   ValuesMap#{Key => #{}};
                               true ->
                                   ValuesMap
                           end
                   end,
    gen_keysmap(
        Tail,
        KeysMap#{Pos => Key},
        Pos + 1,
        NewValuesMap
    ).

%%--------------------------------------------------------------------
%% @doc
%% Generates values map from rest of lines from csv file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_valuesmap(NewLine, KeysMap, AccValuesMap, AccDiffValuesMap, AccKeysSet, Pos) -> {ValuesMap, DiffValuesMap, KeysSet} when
    NewLine :: [csv_to_object:csv_line()],
    KeysMap :: keys_map(),
    AccValuesMap :: lang_map(),
    AccDiffValuesMap :: AccValuesMap,
    AccKeysSet :: nls_file_name_map(),
    Pos :: key_pos(),
    ValuesMap :: AccValuesMap,
    DiffValuesMap :: AccValuesMap,
    KeysSet :: AccKeysSet.
gen_valuesmap([[] | Tail], KeysMap, AccValuesMap, AccDiffValuesMap, AccKeysSet, Pos) ->
    gen_valuesmap(Tail, KeysMap, AccValuesMap, AccDiffValuesMap, AccKeysSet, Pos + 1);
gen_valuesmap([Value | Tail], KeysMap, AccValuesMap, AccDiffValuesMap, AccKeysSet, Pos) ->
    Key = maps:get(Pos, KeysMap),
    {NewKeysMap, UpdatedAccValueMap, UpdatedAccDiffValuesMap, UpdatedAccKeysSet} =
        case Key of
            id ->
                Id = list_to_atom(Value),
                {
                    KeysMap#{
                        cur_id => Id
                    },
                    AccValuesMap,
                    AccDiffValuesMap,
                    gb_sets:add(Id, AccKeysSet)
                };
            Lang ->
                Id = maps:get(cur_id, KeysMap),
                LangMap = maps:get(Lang, AccValuesMap),
                ExistingValue = maps:get(Id, LangMap, undefined),
                NewValue = re:replace(Value, "~n", "\n", [global, {return, binary}]),
                {
                    KeysMap,
                    AccValuesMap#{
                        Lang := LangMap#{
                            Id => NewValue
                        }
                    },
                    if
                        ExistingValue =:= NewValue -> % undefined == ExistingValue orelse
                            AccDiffValuesMap;
                        true ->
                            {NewAccDiffValuesMap, ExistingLangMap} =
                                case maps:get(Lang, AccDiffValuesMap, undefined) of
                                    undefined ->
                                        NewLangMap = #{},
                                        {
                                            AccDiffValuesMap#{
                                                Lang => NewLangMap
                                            },
                                            NewLangMap
                                        };
                                    LangMapExist ->
                                        {AccDiffValuesMap, LangMapExist}
                                end,
                            NewAccDiffValuesMap#{
                                Lang := ExistingLangMap#{
                                    Id => NewValue
                                }
                            }
                    end,
                    AccKeysSet
                }
        end,
    gen_valuesmap(Tail, NewKeysMap, UpdatedAccValueMap, UpdatedAccDiffValuesMap, UpdatedAccKeysSet, Pos + 1);
gen_valuesmap([], _KeysMap, ValueMap, DiffValuesMap, KeysSet, _Pos) ->
    {ValueMap, DiffValuesMap, KeysSet}.

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
fill_in_nls([], _LangMap, AccContentList) ->
    lists:reverse(AccContentList);
fill_in_nls([{nls, NlsKey} | Tail], LangMap, AccContentList) ->
    fill_in_nls(Tail, LangMap, [maps:get(NlsKey, LangMap) | AccContentList]);
fill_in_nls([{nls, NlsKey, Replacements} | Tail], LangMap, AccContentList) ->
    ConvertedReplacements = fill_in_nls(Replacements, LangMap, []),
    ReplacedContent = fill_in_content(maps:get(NlsKey, LangMap), ConvertedReplacements, <<>>),
    fill_in_nls(Tail, LangMap, [ReplacedContent | AccContentList]);
fill_in_nls([{NlsContent, Replacements} | Tail], LangMap, AccContentList) ->
    ConvertedReplacements = fill_in_nls(Replacements, LangMap, []),
    ReplacedContent = fill_in_content(NlsContent, ConvertedReplacements, <<>>),
    fill_in_nls(Tail, LangMap, [ReplacedContent | AccContentList]);
fill_in_nls([NonNlsKey | Tail], LangMap, AccContentList) ->
    fill_in_nls(Tail, LangMap, [cm:to_binary(NonNlsKey) | AccContentList]).

%%--------------------------------------------------------------------
%% @doc
%% Reads nls values from csv file and return nls map.
%% This function is called by lists:foldl/3 or lists:foldr/3.
%% @see lists:foldl/3.
%% @see lists:foldr/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_nls_file(NlsFilePath, {AccNlsMap, AccDiffNlsMap, AccNlsFileNameMap}) -> {NlsMap, DiffNlsMap, NlsFileNameMap} when
    NlsFilePath :: file:name_all(),
    AccNlsMap :: nls_map(),
    AccDiffNlsMap :: AccNlsMap,
    AccNlsFileNameMap :: nls_file_name_map(),
    NlsMap :: AccNlsMap,
    DiffNlsMap :: AccNlsMap,
    NlsFileNameMap :: AccNlsFileNameMap.
read_nls_file(NlsFilePath, {AccNlsMap, AccDiffNlsMap, AccNlsFileNameMap}) ->
    {ok, NlsFile} = file:open(NlsFilePath, [read]),
    {ok, {NlsMap, DiffNlsMap, FileKeysSet}} = ecsv:process_csv_file_with(NlsFile, fun read_line/2, {0, AccNlsMap, AccDiffNlsMap}),
    ok = file:close(NlsFile),
    {NlsMap, DiffNlsMap, AccNlsFileNameMap#{
        list_to_atom(filename:rootname(filename:basename(NlsFilePath))) => FileKeysSet
    }}.