%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Common API module. This module provides APIs that handles generic handlings.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(cm).
-author("Shuieryin").

%% API
-export([
    is_module_exist/1,
    type_of/1,
    timestamp/0,
    hot_code_replace/1,
    index_of/2,
    until_process_terminated/1,
    increase_vsn/3,
    q/0,
    first_to_lower/1,
    remove_last_newline/1,
    random_from_list/1,
    observer/0,
    binary_join/2,
    type_values/2,
    uuid/0,
    rr/2,
    parse_target_id/1,
    general_target/2,
    execute_command/2,
    execute_sync_command/2,
    rb/0,
    module_src_path/1,
    pp/1,
    show_errors/1,
    collect_record_value/4,
    strings_to_atoms/1,
    binaries_to_atoms/1,
    update_record_value/3,
    f2i/2,
    to_binary/1,
    app_name/0,
    remove_record_fields/3,
    add_record_fields/4,
    retrieve_n_break/2,
    str_to_term/1,
    cmd/1
]).

-type valid_type() :: atom | binary | bitstring | boolean | float | function | integer | list | pid | port | reference | tuple | map.

-define(OBSERVER_NODE, 'macbook@192.168.1.110').

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../data_type/scene_info.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check if module exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_module_exist(Module) -> boolean() when
    Module :: module().
is_module_exist(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _ErrorType:_Reason ->
                    false
            end;

        false ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Detect value type.
%%
%% @end
%%--------------------------------------------------------------------
-spec type_of(X) -> Result when
    X :: term(), % generic term
    Result :: valid_type() | unknown.
type_of(X) when is_integer(X) -> integer;
type_of(X) when is_float(X) -> float;
type_of(X) when is_list(X) -> list;
type_of(X) when is_tuple(X) -> tuple;
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_boolean(X) -> boolean;
type_of(X) when is_function(X) -> function;
type_of(X) when is_pid(X) -> pid;
type_of(X) when is_port(X) -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X) -> atom;
type_of(X) when is_map(X) -> map;
type_of(_X) -> unknown.

%%--------------------------------------------------------------------
%% @doc
%% Return timestamp in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> Timestamp when
    Timestamp :: pos_integer(). % generic integer
timestamp() ->
    {Hour, Minute, _Second} = os:timestamp(),
    Hour * 1000000 + Minute.

%%--------------------------------------------------------------------
%% @doc
%% Hot code replace modules by "ModuleNameList".
%%
%% @end
%%--------------------------------------------------------------------
-spec hot_code_replace(ModuleNameList) -> Result when
    ModuleName :: module(),
    ModuleNameList :: [ModuleName],
    Result :: [code:load_ret()].
hot_code_replace(ModuleNameList) ->
    [begin code:purge(ModuleName), code:load_file(ModuleName) end || ModuleName <- ModuleNameList].

%%--------------------------------------------------------------------
%% @doc
%% Finds the element position from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec index_of(Item, List) -> Pos when
    List :: [term()], % generic term
    Item :: term(), % generic term
    Pos :: -1 | pos_integer(). % generic integer
index_of(Item, List) ->
    index_of(Item, List, 1).

%%--------------------------------------------------------------------
%% @doc
%% Checks if pid or register name process still exists in "DetectPeriodInMilli"
%% milliseconds and return ok until the target is terminated. Use this
%% function in extreme caution! Only when you are 100% sure that the
%% target process is going to be terminated otherwise this function never returns.
%%
%% @end
%%--------------------------------------------------------------------
-spec until_process_terminated(PidOrName) -> ok when
    PidOrName :: erlang:monitor_process_identifier().
until_process_terminated(PidOrName) ->
    if
        PidOrName /= undefined ->
            MonitorRef = monitor(process, PidOrName),
            receive
                {'DOWN', MonitorRef, process, _Pid, _Reason} ->
                    ok
            end;
        true ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function increases version number
%%
%% @end
%%--------------------------------------------------------------------
-spec increase_vsn(SourceVersion, VersionDepth, Increment) -> UpgradedVersion when
    SourceVersion :: string(),
    VersionDepth :: pos_integer(), % generic integer
    Increment :: pos_integer(), % generic integer
    UpgradedVersion :: SourceVersion.
increase_vsn(SourceVersion, VersionDepth, Increment) ->
    string:join(increase_vsn(string:tokens(SourceVersion, "."), VersionDepth, Increment, 1, []), ".").

%%--------------------------------------------------------------------
%% @doc
%% Terminate redis-server and the erlang server properly by committing
%% all possible states.
%%
%% @end
%%--------------------------------------------------------------------
-spec q() -> no_return().
q() ->
    ok = login_server:logout_all_players(),
    rb:stop(),
    os:cmd("redis-cli shutdown"),
    init:stop().

%%--------------------------------------------------------------------
%% @doc
%% Lowercases the first letter of give string.
%%
%% @end
%%--------------------------------------------------------------------
-spec first_to_lower(SrcString) -> FirstLoweredString when
    SrcString :: string(),
    FirstLoweredString :: SrcString.
first_to_lower([First | Rest] = SrcString) when is_list(SrcString) ->
    FirstLowered = string:to_lower([First]),
    FirstLowered ++ Rest.

%%--------------------------------------------------------------------
%% @doc
%% Removes last new line binary in List.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_last_newline(SrcList) -> RetList when
    SrcList :: [nls_server:value()],
    RetList :: SrcList.
remove_last_newline(SrcList) ->
    case lists:reverse(SrcList) of
        [<<"\n">> | Rest] ->
            lists:reverse(Rest);
        _NonNeedRemove ->
            SrcList
    end.

%%--------------------------------------------------------------------
%% @doc
%% Random pick element from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec random_from_list(SrcList) -> Element when
    Element :: term() | undefined, % generic term
    SrcList :: [Element].
random_from_list([]) ->
    undefined;
random_from_list(SrcList) ->
    ListSize = length(SrcList),
    RandomPos = random:uniform(ListSize),
    lists:nth(RandomPos, SrcList).

%%--------------------------------------------------------------------
%% @doc
%% Connect observer node.
%%
%% @end
%%--------------------------------------------------------------------
-spec observer() -> pong | pang.
observer() ->
    net_adm:ping(?OBSERVER_NODE).

%%--------------------------------------------------------------------
%% @doc
%% Binary join with separator.
%%
%% @end
%%--------------------------------------------------------------------
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(
        fun(A, B) ->
            if
                bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                true -> A
            end
        end,
        <<>>,
        List
    ).

%%--------------------------------------------------------------------
%% @doc
%% Get type values if split by "|".
%%
%% For example:
%%
%%          Given type in nls_server: -ty¢pe support_lang() :: zh | en.
%%
%%          [zh, en] = type_values(nls_server, support_lang).
%%
%% @end
%%--------------------------------------------------------------------
-spec type_values(ModuleName, TypeName) -> TypeValues when
    ModuleName :: module(),
    TypeName :: atom(), % generic atom
    TypeValues :: term(). % generic term
type_values(ModuleName, TypeName) ->
    ModulePath = module_src_path(ModuleName),
    {ok, AbstractCode} = dialyzer_utils:get_abstract_code_from_src(ModulePath),
    {ok, TypeDict} = dialyzer_utils:get_record_and_type_info(AbstractCode),

    TypeKey = {type, TypeName, 0},
    case dict:is_key(TypeKey, TypeDict) of
        true ->
            {{ModuleName, {ModulePath, _SigNum}, RawValues, []}, any}
                = dict:fetch(TypeKey, TypeDict),

            case RawValues of
                {type, _SigNum, _ListType, TypeList} ->
                    [TypeAtom || {_TypeType, _TypeSigNum, TypeAtom} <- TypeList];
                {_TypeType, _SigNum, TypeAtom} ->
                    [TypeAtom]
            end;
        false ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Generate uuid in atom.
%% Caution! Be careful of this function. Improper use may lead to memory leak.
%%
%% @end
%%--------------------------------------------------------------------
-spec uuid() -> atom(). % generic atom
uuid() ->
    list_to_atom(uuid:uuid_to_string(uuid:get_v4())).

%%--------------------------------------------------------------------
%% @doc
%% Random float range. Input arguments must be integer.
%%
%% @end
%%--------------------------------------------------------------------
-spec rr(Start, End) -> float() when
    Start :: pos_integer(),
    End :: Start.
rr(Start, End) ->
    Value = if
                Start > End ->
                    Start;
                true ->
                    Start + random:uniform(End - Start)
            end,
    Value / 100.

%%--------------------------------------------------------------------
%% @doc
%% TargetArgs is converted from
%%        binary "little boy 2" to "TargetId=little_boy" and "Sequence=2".
%%        binary "shuieryin" to "TargetId=collin" and "Sequence=1".
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_target_id(TargetArgs) -> {ok, TargetId, Sequence} when
    TargetArgs :: binary(),
    TargetId :: player_fsm:id() | npc_fsm:npc_id(),
    Sequence :: non_neg_integer().
parse_target_id(TargetArgs) ->
    [RawSequence | Rest] = lists:reverse(re:split(TargetArgs, <<" ">>)),
    {TargetId, Sequence} =
        case Rest of
            [] ->
                {RawSequence, 1};
            Rest ->
                case re:run(RawSequence, "^[0-9]*$") of
                    {match, _Captured} ->
                        {cm:binary_join(lists:reverse(Rest), <<"_">>), binary_to_integer(RawSequence)};
                    nomatch ->
                        {re:replace(TargetArgs, <<" ">>, <<"_">>, [global, {return, binary}]), 1}
                end
        end,
    {ok, TargetId, Sequence}.

%%--------------------------------------------------------------------
%% @doc
%% This function is for skipping the implementation of from player initialization
%% stage and usually called by command exec function.
%%
%% @end
%%--------------------------------------------------------------------
-spec general_target(Uid, CommandContext) -> ok when
    Uid :: player_fsm:uid(),
    CommandContext :: #command_context{}.
general_target(Uid, CommandContext) ->
    gen_fsm:send_event(Uid, {general_target, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% This is a general function for executing command in different states.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_command(Uid, CommandContext) -> ok when
    Uid :: player_fsm:uid() | npc_fsm:npc_uid() | scene_fsm:scene_name(),
    CommandContext :: #command_context{}.
execute_command(Uid, CommandContext) ->
    gen_fsm:send_event(Uid, {execute_command, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% This is a general function for executing command in different states
%% for getting result.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_sync_command(ProcessName, CommandContext) -> Result when
    ProcessName :: player_fsm:uid() | npc_fsm:npc_uid() | scene_fsm:scene_name(),
    CommandContext :: #command_context{},
    Result :: term(). % generic term
execute_sync_command(ProcessName, CommandContext) ->
    gen_fsm:sync_send_event(ProcessName, {execute_command, CommandContext}).

%%--------------------------------------------------------------------
%% @doc
%% Random boolean value.
%%
%% @end
%%--------------------------------------------------------------------
-spec rb() -> boolean().
rb() ->
    random:uniform() > 0.499.

%%--------------------------------------------------------------------
%% @doc
%% Get source path by module name.
%%
%% @end
%%--------------------------------------------------------------------
-spec module_src_path(ModuleName) -> SrcPath when
    ModuleName :: module(),
    SrcPath :: file:filename().
module_src_path(ModuleName) ->
    get_module_src_path(ModuleName:module_info(compile)).

%%--------------------------------------------------------------------
%% @doc
%% Pretty print binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec pp(ReturnContentBinary) -> ok when
    ReturnContentBinary :: binary().
pp(ReturnContentBinary) ->
    Content = re:replace(ReturnContentBinary, <<"\n">>, <<"~n">>, [global, {return, binary}]),
    NewLine = <<"~n">>,
    error_logger:info_msg(unicode:characters_to_list(<<Content/binary, NewLine/binary>>)).

%%--------------------------------------------------------------------
%% @doc
%% Show show last given number of errors.
%%
%% @end
%%--------------------------------------------------------------------
-spec show_errors(Limit) -> ok when
    Limit :: non_neg_integer().
show_errors(Limit) when is_integer(Limit) ->
    rb:start([{type, [error_report, error]}, {max, Limit}]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Collect record value and put it in binding for undefined value only.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_record_value(RecordFieldNames, Record, NewFieldNames, ExistingFieldBindings) -> UpdatedFieldBindings when
    RecordFieldNames :: [atom()], % generic atom
    Record :: tuple(), % generic tuple
    NewFieldNames :: erl_eval:bindings(),
    ExistingFieldBindings :: erl_eval:bindings(),
    UpdatedFieldBindings :: ExistingFieldBindings.
collect_record_value(RecordFieldNames, Record, NewFieldNames, ExistingFieldBindings) ->
    [_RecordName | DataList] = tuple_to_list(Record),
    do_collect_record_value(RecordFieldNames, DataList, NewFieldNames, ExistingFieldBindings).

%%--------------------------------------------------------------------
%% @doc
%% Convert list of strings to list of atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec strings_to_atoms(StringList) -> AtomList when
    StringList :: [string()],
    AtomList :: [atom()]. % generic atom
strings_to_atoms(StringList) ->
    [list_to_atom(String) || String <- StringList].

%%--------------------------------------------------------------------
%% @doc
%% Convert list of strings to list of atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec binaries_to_atoms(BinaryList) -> AtomList when
    BinaryList :: [binary()],
    AtomList :: [atom()]. % generic atom
binaries_to_atoms(StringList) ->
    [binary_to_atom(Bin, utf8) || Bin <- StringList].

%%--------------------------------------------------------------------
%% @doc
%% Update record values.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_record_value(RecordFieldNames, Record, NewValueBindings) -> UpdatedRecord when
    RecordFieldNames :: [atom()], % generic atom
    Record :: tuple(), % generic tuple
    NewValueBindings :: erl_eval:bindings(),
    UpdatedRecord :: Record.
update_record_value(RecordFieldNames, Record, NewValueBindings) ->
    [RecordName | ExistingDataList] = tuple_to_list(Record),
    UpdatedDataList = do_update_record_value(RecordFieldNames, ExistingDataList, NewValueBindings, []),
    list_to_tuple([RecordName | UpdatedDataList]).

%%--------------------------------------------------------------------
%% @doc
%% Remove record fields.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_record_fields(RecordFieldNames, Record, FieldNamesToBeRemoved) -> UpdatedRecord when
    RecordFieldNames :: [atom()], % generic atom
    Record :: tuple(), % generic tuple
    FieldNamesToBeRemoved :: RecordFieldNames,
    UpdatedRecord :: Record.
remove_record_fields(RecordFieldNames, Record, FieldNamesToBeRemoved) ->
    [RecordName | ExistingDataList] = tuple_to_list(Record),
    UpdatedDataList = do_remove_record_fields(RecordFieldNames, ExistingDataList, FieldNamesToBeRemoved, []),
    list_to_tuple([RecordName | UpdatedDataList]).

%%--------------------------------------------------------------------
%% @doc
%% Add record fields.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_record_fields(OldRecordFieldNames, NewRecordFieldNames, Record, NewValueBindings) -> UpdatedRecord when
    OldRecordFieldNames :: [atom()], % generic atom
    NewRecordFieldNames :: OldRecordFieldNames,
    Record :: tuple(), % generic tuple
    NewValueBindings :: erl_eval:bindings(),
    UpdatedRecord :: Record.
add_record_fields(OldRecordFieldNames, NewRecordFieldNames, Record, NewValueBindings) ->
    [RecordName | ExistingDataList] = tuple_to_list(Record),
    UpdatedDataList = do_add_record_fields(OldRecordFieldNames, NewRecordFieldNames, ExistingDataList, NewValueBindings, []),
    list_to_tuple([RecordName | UpdatedDataList]).

%%--------------------------------------------------------------------
%% @doc
%% Convert float to integer.
%%
%% @end
%%--------------------------------------------------------------------
-spec f2i(float(), integer()) -> integer().
f2i(Float, Min) ->
    IntVal = if
                 is_float(Float) ->
                     list_to_integer(float_to_list(Float, [{decimals, 0}]));
                 is_integer(Float) ->
                     Float;
                 true ->
                     Min
             end,
    if
        IntVal < Min ->
            Min;
        true ->
            IntVal
    end.

%%--------------------------------------------------------------------
%% @doc
%% Convert generic term to readable binary
%%
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Term) -> Binary when
    Term :: term(), % generic term
    Binary :: binary().
to_binary(Term) when is_binary(Term) ->
    Term;
to_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) ->
    integer_to_binary(Term);
to_binary(Term) when is_float(Term) ->
    float_to_binary(Term);
to_binary(Term) when is_list(Term) ->
    list_to_binary(Term);
to_binary(Term) ->
    term_to_binary(Term).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve current application name.
%%
%% @end
%%--------------------------------------------------------------------
-spec app_name() -> atom(). % generic atom
app_name() ->
    AppNameStr = begin
                     [ProjectPath | _RestPath] = re:split(filename:absname(""), "_build", [{return, list}]),
                     filename:basename(ProjectPath)
                 end,
    list_to_atom(AppNameStr).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve value from List and return the match value once found by
%% omitting rest elements from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec retrieve_n_break(Func, List) -> Elem | undefined when
    Elem :: term(),
    List :: list(),
    Func :: fun((Elem) -> boolean()).
retrieve_n_break(Func, [H | T]) ->
    case Func(H) of
        true ->
            H;
        false ->
            retrieve_n_break(Func, T)
    end;
retrieve_n_break(Func, []) when is_function(Func, 1) -> undefined.

%%--------------------------------------------------------------------
%% @doc
%% Convert string content to term.
%%
%% @end
%%--------------------------------------------------------------------
-spec str_to_term(string()) -> term().
str_to_term(SrcStr) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(SrcStr),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%%--------------------------------------------------------------------
%% @doc
%% Execute command and print output in realtime.
%%
%% @end
%%--------------------------------------------------------------------
-spec cmd(string()) -> ok.
cmd(CmdStr) ->
    OutputNode = erlang:open_port({spawn, CmdStr},
        [stderr_to_stdout, in, exit_status,
            binary, stream, {line, 255}]),

    cmd_receive(OutputNode).

%%--------------------------------------------------------------------
%% @doc
%% Receive func for cmd/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec cmd_receive(port()) -> ok.
cmd_receive(OutputNode) ->
    receive
        {OutputNode, {data, {eol, OutputBin}}} ->
            io:format(<<"~n", OutputBin/binary>>),
            cmd_receive(OutputNode);
        {OutputNode, {exit_status, 0}} ->
            io:format("~n")
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This is sub function of increase_vsn/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec increase_vsn(SourceVersion, VersionDepth, Increment, CurDepth, AccVersion) -> UpgradedVersion when
    SourceVersion :: [string()],
    VersionDepth :: pos_integer(), % generic integer
    Increment :: pos_integer(), % generic integer
    CurDepth :: VersionDepth,
    AccVersion :: SourceVersion,
    UpgradedVersion :: SourceVersion.
increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    UpdatedVersionNum =
        case CurDepth =:= VersionDepth of
            true ->
                integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment);
            false ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]);
increase_vsn([], _VersionDepth, _Increment, _CurDepth, UpgradedVersion) ->
    lists:reverse(UpgradedVersion).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function of module_src_path/1.
%% @see module_src_path/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_module_src_path(SourceCompileInfo) -> SrcPath when
    SourceCompileInfo :: any() | beam_lib:compinfo_entry(),
    SrcPath :: string().
get_module_src_path([{source, SrcPath} | _RestPaths]) ->
    SrcPath;
get_module_src_path([_Other | Rest]) ->
    get_module_src_path(Rest).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for collect_record_value/3.
%% @see collect_record_value/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_collect_record_value(RecordFieldNames, DataList, TargetFieldNames, AccFieldBindings) -> FinalFieldBingdings when
    RecordFieldNames :: [atom()], % generic atom
    DataList :: [term()], % generic term
    TargetFieldNames :: erl_eval:bindings(),
    AccFieldBindings :: erl_eval:bindings(),
    FinalFieldBingdings :: AccFieldBindings.
do_collect_record_value([FieldName | RestRecordFieldNames], [FieldValue | RestDataList], TargetFieldNames, AccFieldBindings) ->
    {UpdatedTargetFieldNames, UpdatedAccFieldBindings}
        = case erl_eval:binding(FieldName, TargetFieldNames) of
              {value, TargetFieldName} ->
                  {
                      erl_eval:del_binding(FieldName, TargetFieldNames),
                      erl_eval:add_binding(TargetFieldName, FieldValue, AccFieldBindings)
                  };
              unbound ->
                  {TargetFieldNames, AccFieldBindings}
          end,
    do_collect_record_value(RestRecordFieldNames, RestDataList, UpdatedTargetFieldNames, UpdatedAccFieldBindings);
do_collect_record_value(_RecordFieldNames, _DataList, [], FinalFieldBingdings) ->
    FinalFieldBingdings;
do_collect_record_value([], [], _TargetFieldNames, FinalFieldBingdings) ->
    FinalFieldBingdings.

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for index_of/2.
%% @see index_of/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec index_of(Item, List, Pos) -> FinalPos when
    Item :: term(), % generic term
    List :: [term()], % generic term
    Pos :: pos_integer(),
    FinalPos :: -1 | pos_integer().
index_of(_Item, [], _Pos) ->
    -1;
index_of(Elem, [Elem | _Tail], Pos) ->
    Pos;
index_of(Item, [_NotMatchItem | Tail], Pos) ->
    index_of(Item, Tail, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for updated_record_value/3.
%% @see updated_record_value/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_update_record_value(RecordFieldNames, ExistingDataList, NewValueBindings, AccDataList) -> UpdatedDataList when
    RecordFieldNames :: [atom()], % generic atom
    ExistingDataList :: [term()], % generic term
    NewValueBindings :: erl_eval:bindings(),
    AccDataList :: ExistingDataList,
    UpdatedDataList :: AccDataList.
do_update_record_value([FieldName | RestRecordFieldNames], [ExistingFieldValue | RestDataList], NewValueBindings, AccDataList) ->
    {UpdatedNewValueBindings, NewFieldValue}
        = case erl_eval:binding(FieldName, NewValueBindings) of
              {value, BindingValue} ->
                  {erl_eval:del_binding(FieldName, NewValueBindings), BindingValue};
              unbound ->
                  {NewValueBindings, ExistingFieldValue}
          end,
    do_update_record_value(RestRecordFieldNames, RestDataList, UpdatedNewValueBindings, [NewFieldValue | AccDataList]);
do_update_record_value(_RecordFieldNames, RestDataList, [], UpdatedDataList) ->
    lists:reverse(UpdatedDataList) ++ RestDataList;
do_update_record_value([], [], _NewValueBingdings, UpdatedDataList) ->
    lists:reverse(UpdatedDataList).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for remove_record_fields/3.
%% @see remove_record_fields/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_remove_record_fields(RecordFieldNames, ExistingDataList, FieldNamesToBeRemoved, AccDataList) -> UpdatedDataList when
    RecordFieldNames :: [atom()], % generic atom
    ExistingDataList :: [term()], % generic term
    FieldNamesToBeRemoved :: RecordFieldNames,
    AccDataList :: ExistingDataList,
    UpdatedDataList :: AccDataList.
do_remove_record_fields([FieldName | RestRecordFieldNames], [ExistingFieldValue | RestDataList], FieldNamesToBeRemoved, AccDataList) ->
    {UpdatedAccDataList, UpdatedFieldNamesToBeRemoved} =
        case lists:member(FieldName, FieldNamesToBeRemoved) of
            true ->
                {AccDataList, lists:delete(FieldName, FieldNamesToBeRemoved)};
            false ->
                {[ExistingFieldValue | AccDataList], FieldNamesToBeRemoved}
        end,
    do_remove_record_fields(RestRecordFieldNames, RestDataList, UpdatedFieldNamesToBeRemoved, UpdatedAccDataList);
do_remove_record_fields(_RecordFieldNames, RestDataList, [], UpdatedDataList) ->
    lists:reverse(UpdatedDataList) ++ RestDataList;
do_remove_record_fields([], [], _FieldNamesToBeRemoved, UpdatedDataList) ->
    lists:reverse(UpdatedDataList).

%%--------------------------------------------------------------------
%% @doc
%% Implementation function for add_record_fields/4.
%% @see add_record_fields/4.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_add_record_fields(OldRecordFieldNames, NewRecordFieldNames, ExistingDataList, NewValueBindings, AccDataList) -> UpdatedDataList when
    OldRecordFieldNames :: [atom()], % generic atom
    NewRecordFieldNames :: OldRecordFieldNames,
    ExistingDataList :: [term()], % generic term
    NewValueBindings :: erl_eval:bindings(),
    AccDataList :: ExistingDataList,
    UpdatedDataList :: AccDataList.
do_add_record_fields([FieldName | RestOldRecordFieldNames], [FieldName | RestNewRecordFieldNames], [ExistingFieldValue | RestDataList], NewValueBindings, AccDataList) ->
    do_add_record_fields(RestOldRecordFieldNames, RestNewRecordFieldNames, RestDataList, NewValueBindings, [ExistingFieldValue | AccDataList]);
do_add_record_fields(OldRecordFieldNames, [FieldName | RestNewRecordFieldNames], RestDataList, NewValueBindings, AccDataList) ->
    {UpdatedNewValueBindings, NewFieldValue}
        = case erl_eval:binding(FieldName, NewValueBindings) of
              {value, BindingValue} ->
                  {erl_eval:del_binding(FieldName, NewValueBindings), BindingValue};
              unbound ->
                  {NewValueBindings, undefined}
          end,
    do_add_record_fields(OldRecordFieldNames, RestNewRecordFieldNames, RestDataList, UpdatedNewValueBindings, [NewFieldValue | AccDataList]);
do_add_record_fields(_OldRecordFieldNames, _NewRecordFieldNames, RestDataList, [], UpdatedDataList) ->
    lists:reverse(UpdatedDataList) ++ RestDataList;
do_add_record_fields([], _NewRecordFieldNames, [], _NewValueBingdings, UpdatedDataList) ->
    lists:reverse(UpdatedDataList).