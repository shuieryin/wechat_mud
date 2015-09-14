%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 九月 2015 下午12:22
%%%-------------------------------------------------------------------
-module(nls_server).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([start_link/1,
    read_line/2,
    get/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2]).

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

get(ServerName, NlsKey, Lang) ->
    gen_server:call(ServerName, {get, NlsKey, Lang}).

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
read_line({newline, NewLine}, 0) ->
    {1, gen_keysmap(NewLine, #{}, 0), #{}};
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
    {ok, ValuesMap} = ecsv:process_csv_file_with(File, fun read_line/2, 0),
    {ok, ValuesMap}.

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

    Request :: {get, NlsKey, Lang},
    NlsKey :: atom(),
    Lang :: atom(),
    From :: {pid(), Tag :: term()},
    Reply :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_call({get, NlsKey, Lang}, _From, State) ->
    {reply, maps:get(Lang, maps:get(NlsKey, State)), State}.

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

    Request :: term(),
    State :: map(),
    NewState :: map(),
    Reason :: term().
handle_cast(_Request, State) ->
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
-spec gen_keysmap(NewLine, KeysMap, Pos) -> FinalKeysMap when
    NewLine :: [term()],
    KeysMap :: map(),
    Pos :: non_neg_integer(),
    FinalKeysMap :: map().
gen_keysmap([], KeysMap, _) ->
    KeysMap;
gen_keysmap([Key | Tail], KeysMap, Pos) ->
    gen_keysmap(Tail, KeysMap#{Pos => Key}, Pos + 1).

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
    Key = list_to_atom(maps:get(Pos, KeysMap)),
    {NewKeysMap, NewValueMap} = case Key of
                                    id ->
                                        Id = list_to_atom(Value),
                                        {KeysMap#{cur_id => Id}, ValuesMap#{Id => #{}}};
                                    Lang ->
                                        Id = maps:get(cur_id, KeysMap),
                                        ItemMap = maps:get(Id, ValuesMap),
                                        {KeysMap, ValuesMap#{Id := ItemMap#{Lang => unicode:characters_to_binary(Value)}}}
                                end,
    gen_valuesmap(Tail, NewKeysMap, NewValueMap, Pos + 1).
