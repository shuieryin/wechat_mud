%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Converts csv files to object configs according to function
%%% provided from caller.
%%%
%%% @end
%%% Created : 04. Nov 2015 9:13 PM
%%%-------------------------------------------------------------------
-module(csv_to_object).
-author("shuieryin").

%% API
-export([
    traverse_merge_files/4,
    traverse_files/3,
    traverse_files/4,
    parse_file/2,
    parse_file/3,
    convert_priv_paths/1
]).

-define(FILE_EXTENSION, ".csv").
-define(NAME_TYPE_SEPARATOR, ":").

-type key() :: atom(). % generic atom
-type value() :: term(). % generic term
-type field_type() :: atom(). % generic atom
-type csv_line() :: string().
-type csv_row_data() :: tuple(). % generic tuple
-type csv_data() :: #{key() => csv_row_data()}.
-type field_info() :: {key(), field_type()}.
-type field_infos() :: [field_info()].
-type csv_object() :: #{key() => csv_data()}.
-type csv_data_struct() :: {module(), [atom()]} | module() | {module(), [atom()], function()}. % generic atom

-include("../data_type/player_profile.hrl").

-export_type([
    csv_data/0,
    csv_object/0,
    csv_data_struct/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder and merge as one map.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_merge_files(FilePathList, AccValuesMap, ExistingValuesMap, RowFun) -> {ValuesMap, ChangedValuesMap} when
    FilePathList :: [file:filename_all()],
    RowFun :: function(),
    ValuesMap :: csv_data(),
    AccValuesMap :: ValuesMap,
    ExistingValuesMap :: ValuesMap,
    ChangedValuesMap :: ValuesMap.
traverse_merge_files(FilePathList, AccValuesMap, ExistingValuesMap, RowFun) ->
    {ValuesMap, ChangedValuesMap, _ExistingValuesMap} =
        lists:foldl(
            fun(FilePath, {AccValues, AccChangedValues, ExistingValues}) ->
                FileName = filename:basename(FilePath),
                case filename:extension(FileName) == ?FILE_EXTENSION of
                    true ->
                        {Values, ChangedValues} = parse_file(FilePath, ExistingValues, RowFun),
                        NewAccValues = maps:merge(AccValues, Values),
                        NewAccChangedValues = maps:merge(AccChangedValues, ChangedValues),
                        {NewAccValues, NewAccChangedValues, ExistingValues};
                    false ->
                        {AccValues, AccChangedValues, ExistingValues}
                end
            end,
            {AccValuesMap, #{}, ExistingValuesMap},
            FilePathList),
    {ValuesMap, ChangedValuesMap}.

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder with default
%% row function.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_files(FilePathList, AccMapFromFiles, AccChangedMapFromFiles) -> {MapFromFiles, ChangedMapFromFiles, DeletedFilesStruct} when
    FilePathList :: [file:name_all()],
    AccMapFromFiles :: csv_data(),
    MapFromFiles :: AccMapFromFiles,
    AccChangedMapFromFiles :: AccMapFromFiles,
    ChangedMapFromFiles :: AccMapFromFiles,
    DeletedFilesStruct :: [csv_data_struct()].
traverse_files(FilePathList, AccMapFromFiles, AccChangedMapFromFiles) ->
    traverse_files(FilePathList, AccMapFromFiles, AccChangedMapFromFiles, fun default_row_fun/1).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_files(FilePathList, AccMapFromFiles, AccChangedMapFromFiles, RowFun) -> {MapFromFiles, ChangedMapFromFiles, DeletedFilesStruct} when
    FilePathList :: [file:name_all()],
    RowFun :: function(),
    AccMapFromFiles :: csv_data(),
    AccChangedMapFromFiles :: AccMapFromFiles,
    MapFromFiles :: AccMapFromFiles,
    ChangedMapFromFiles :: AccMapFromFiles,
    DeletedFilesStruct :: [csv_data_struct()].
traverse_files(FilePathList, AccMapFromFiles, AccChangedMapFromFiles, RowFun) ->
    {MapFromFiles, ChangedMapFromFiles, DeletedFilesStruct} = lists:foldl(
        fun(FilePath, {AccAccMapFromFiles, AccAccChangedMapFromFiles, AccDeletedFilesStruct}) ->
            FileName = filename:basename(FilePath),
            case filename:extension(FileName) == ?FILE_EXTENSION of
                true ->
                    Key = list_to_atom(filename:rootname(FileName)),
                    ExistingChangedValuesMap = maps:get(Key, AccMapFromFiles, #{}),
                    {ValuesMap, ChangedValuesMap} = parse_file(FilePath, ExistingChangedValuesMap, RowFun),
                    {
                        AccAccMapFromFiles#{
                            Key => ValuesMap
                        },

                        case maps:size(ChangedValuesMap) =:= 0 of
                            true ->
                                AccAccChangedMapFromFiles;
                            false ->
                                AccAccChangedMapFromFiles#{
                                    Key => ChangedValuesMap
                                }
                        end,

                        case maps:keys(maps:without(maps:keys(ValuesMap), ExistingChangedValuesMap)) of
                            [] ->
                                AccDeletedFilesStruct;
                            DeletedRecordNames ->
                                [{Key, DeletedRecordNames} | AccDeletedFilesStruct]
                        end
                    };
                false ->
                    {AccAccMapFromFiles, AccAccChangedMapFromFiles}
            end
        end,
        {AccMapFromFiles, AccChangedMapFromFiles, []},
        FilePathList),
    {MapFromFiles, ChangedMapFromFiles, DeletedFilesStruct}.

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs with default row function from file which
%% the number of object configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath, ExistingChangedValuesMap) -> {ValuesMap, ChangedValuesMap} when
    FilePath :: file:filename_all(),
    ValuesMap :: csv_data(),
    ChangedValuesMap :: ValuesMap,
    ExistingChangedValuesMap :: ValuesMap.
parse_file(FilePath, ExistingChangedValuesMap) ->
    parse_file(FilePath, ExistingChangedValuesMap, fun default_row_fun/1).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from file which the number of object
%% configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath, ExistingChangedValuesMap, RowFun) -> {ValuesMap, ChangedValuesMap} when
    FilePath :: file:filename(),
    RowFun :: function(),
    ValuesMap :: csv_data(),
    ExistingChangedValuesMap :: ValuesMap,
    ChangedValuesMap :: ValuesMap.
parse_file(FilePath, ExistingChangedValuesMap, RowFun) ->
    {ok, File} = file:open(FilePath, [read]),
    RecordName = list_to_atom(filename:rootname(filename:basename(FilePath))),
    {ok, {ValuesMap, ChangedValuesMap}} = ecsv:process_csv_file_with(File, fun traverse_rows/2, {0, RowFun, RecordName, ExistingChangedValuesMap}),
    ok = file:close(File),
    {ValuesMap, ChangedValuesMap}.

%%--------------------------------------------------------------------
%% @doc
%% List changed priv file paths for hot code upgrade before committed.
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_priv_paths({ModifiedFiles, AddedFiles, DeletedFiles}) -> no_change | {ModifiedFilePaths, AddedFilePaths, DeletedFileNames} when
    ModifiedFiles :: [file:filename_all()],
    AddedFiles :: ModifiedFiles,
    DeletedFiles :: ModifiedFiles,
    ModifiedFilePaths :: ModifiedFiles,
    AddedFilePaths :: ModifiedFiles,
    DeletedFileNames :: [atom()]. % file name % generic atom
convert_priv_paths({ModifiedFiles, AddedFiles, DeletedFiles}) ->
    BasePath = filename:dirname(code:priv_dir(cm:app_name())),

    if
        ModifiedFiles == [] andalso AddedFiles == [] andalso DeletedFiles == [] ->
            no_change;
        true ->
            ModifiedFilePaths = [filename:join(BasePath, ModifiedFile) || ModifiedFile <- ModifiedFiles],
            AddedFilePaths = [filename:join(BasePath, AddedFile) || AddedFile <- AddedFiles],
            DeletedFileNames = [list_to_atom(filename:basename(filename:rootname(DeletedFile))) || DeletedFile <- DeletedFiles],
            {ModifiedFilePaths, AddedFilePaths, DeletedFileNames}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads line from csv file
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_rows(NewRowData, State) -> {RowValuesMap, ChangedRowValuesMap} when
    NewRowData :: {newline, [csv_line()]} | {eof},
    RowFun :: function(),
    RowValuesMap :: csv_data(),
    ChangedRowValuesMap :: RowValuesMap,
    Counter :: non_neg_integer(), % generic integer
    FieldInfos :: field_infos(),
    AccRowValuesMap :: RowValuesMap,
    AccChangedRowValuesMap :: RowValuesMap,
    RecordName :: atom(), % generic atom
    State ::
    {Counter, RowFun, FieldInfos, AccRowValuesMap, AccChangedRowValuesMap, ExistingChangedValuesMap, RecordName} |
    {0, RowFun, RecordName, ExistingChangedValuesMap}.
traverse_rows({newline, NewRow}, {Counter, RowFun, FieldInfos, AccRowValuesMap, AccChangedRowValuesMap, ExistingChangedValuesMap, RecordName}) ->
    [_UselessHead, CurRowKey | _RestCurRowValues] =
        CurRowValues = traverse_column(NewRow, FieldInfos, [RecordName]),
    {UpdatedAccRowValuesMap, UpdatedAccChangedRowValuesMap} =
        case CurRowKey of
            undefined ->
                {AccRowValuesMap, AccChangedRowValuesMap};
            _CurRowKey ->
                case RowFun(CurRowValues) of
                    undefined ->
                        {AccRowValuesMap, AccChangedRowValuesMap};
                    ConvertedCurRowValues ->
                        ExistingRowValues = maps:get(CurRowKey, ExistingChangedValuesMap, undefined),
                        {
                            AccRowValuesMap#{
                                CurRowKey => ConvertedCurRowValues
                            },
                            case ExistingRowValues == ConvertedCurRowValues of
                                true ->
                                    AccChangedRowValuesMap;
                                false ->
                                    AccChangedRowValuesMap#{
                                        CurRowKey => ConvertedCurRowValues
                                    }
                            end
                        }
                end
        end,
    {Counter + 1, RowFun, FieldInfos, UpdatedAccRowValuesMap, UpdatedAccChangedRowValuesMap, ExistingChangedValuesMap, RecordName};
traverse_rows({newline, NewRow}, {0, RowFun, RecordName, ExistingChangedValuesMap}) ->
    FieldInfos = gen_fieldinfos(NewRow, []),
    {1, RowFun, FieldInfos, #{}, #{}, ExistingChangedValuesMap, RecordName};
traverse_rows({eof}, {_Counter, _RowFun, _FieldInfos, RowValuesMap, ChangedRowValuesMap, _ExistingChangedValuesMap, _RecordName}) ->
    {RowValuesMap, ChangedRowValuesMap}.

%%--------------------------------------------------------------------
%% @doc
%% Generates field infos from first row
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_fieldinfos(NewRow, AccFieldInfos) -> FieldInfos when
    NewRow :: [csv_line()],
    AccFieldInfos :: field_infos(),
    FieldInfos :: AccFieldInfos.
gen_fieldinfos([], AccFieldInfos) ->
    lists:reverse(AccFieldInfos);
gen_fieldinfos([RawFieldInfoStr | Tail], AccFieldInfos) ->
    FieldInfo = get_field_name_type(RawFieldInfoStr),
    gen_fieldinfos(Tail, [FieldInfo | AccFieldInfos]).

%%--------------------------------------------------------------------
%% @doc
%% Generates values map from rest of columns of current row.
%%
%% Supported types:
%% integer, float, atom, string, and other terms
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_column(RowData, FieldInfos, AccValues) -> FinalValues when
    RowData :: [csv_line()],
    FieldInfos :: field_infos(),
    AccValues :: [value()],
    FinalValues :: AccValues.
traverse_column([RawValue0 | TailValues], [{_Key, FieldType} | TailFieldInfos], AccValues) ->
    RawValue1 = re:replace(RawValue0, [226, 128, 168], "", [global, {return, list}]), % eliminate new line genereated by Numbers.app
    RawValue2 = re:replace(RawValue1, [226, 128, 156], [34], [global, {return, list}]), % replace special double quote with '\"' from Numbers.app
    RawValue = re:replace(RawValue2, [226, 128, 157], [34], [global, {return, list}]), % replace special double quote with '\"' from Numbers.app
    Value =
        try
            case RawValue of
                [] ->
                    undefined;
                _RawValue ->
                    case FieldType of
                        atom ->
                            list_to_atom(RawValue);
                        binary ->
                            list_to_binary(RawValue);
                        integer ->
                            list_to_integer(RawValue);
                        float ->
                            list_to_float(RawValue);
                        term ->
                            {ok, Tokens, _EndLocation} = erl_scan:string(RawValue),
                            {ok, Term} = erl_parse:parse_term(Tokens),
                            Term;
                        exprs ->
                            {ok, Tokens, _EndLocation} = erl_scan:string(RawValue),
                            {ok, Exprs} = erl_parse:parse_exprs(Tokens),
                            Exprs;
                        skill ->
                            RawValueBin = list_to_binary(RawValue),
                            FromValueNames = collect_formula_value_names(RawValueBin, <<"From">>),
                            ToValueNames = collect_formula_value_names(RawValueBin, <<"To">>),

                            {ok, Tokens, _EndLocation} = erl_scan:string(RawValue),
                            {ok, Exprs} = erl_parse:parse_exprs(Tokens),
                            #skill_formula{
                                formula = Exprs,
                                from_var_names = FromValueNames,
                                to_var_names = ToValueNames
                            };
                        _FieldType ->
                            RawValue
                    end
            end
        catch
            Type:Reason ->
                error_logger:error_msg("traverse_column failed~nRawValue:~tp~nType:~p~nReason:~p~nStackTrace:~p~n", [RawValue, Type, Reason, erlang:get_stacktrace()]),
                RawValue
        end,
    traverse_column(TailValues, TailFieldInfos, [Value | AccValues]);
traverse_column([], _FieldInfos, AccValues) ->
    lists:reverse(AccValues).

%%--------------------------------------------------------------------
%% @doc
%% Collect value names form formula.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_formula_value_names(RawValue, Prefix) -> ValueNames when
    RawValue :: binary(),
    Prefix :: binary(),
    ValueNames :: erl_eval:bindings().
collect_formula_value_names(RawValue, Prefix) ->
    MatchRE = <<"(", Prefix/binary, "_([A-z0-9]*))[\s|.|,]{1}">>,
    case re:run(RawValue, MatchRE, [global, {capture, all_but_first, binary}]) of
        {match, MatchedList} ->
            collect_formula_value_names_convert(MatchedList, erl_eval:new_bindings());
        nomatch ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc
%% Convert matched value names to erl_eval bindings.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_formula_value_names_convert(MatchedList, AccValueNames) -> ValueNames when
    MatchValue :: binary(),
    MatchedList :: [[MatchValue]],
    AccValueNames :: erl_eval:bindings(),
    ValueNames :: AccValueNames.
collect_formula_value_names_convert([[RawBindingKey, RawStatusFieldName] | RestMatchedList], AccValueNames) ->
    UpdatedAccValueNames =
        case erl_eval:binding(RawBindingKey, AccValueNames) of
            unbound ->
                erl_eval:add_binding(
                    binary_to_atom(RawStatusFieldName, utf8),
                    binary_to_atom(RawBindingKey, utf8),
                    AccValueNames
                );
            _Exist ->
                AccValueNames
        end,
    collect_formula_value_names_convert(RestMatchedList, UpdatedAccValueNames);
collect_formula_value_names_convert([], ValueNames) ->
    ValueNames.


%%--------------------------------------------------------------------
%% @doc
%% Default row function which directly return row values map.
%%
%% @end
%%--------------------------------------------------------------------
-spec default_row_fun(RowValues) -> csv_row_data() when
    RowValues :: [value()].
default_row_fun(RowValues) ->
    list_to_tuple(RowValues).

%%--------------------------------------------------------------------
%% @doc
%% Get field name and field type from raw field name info string.
%% "fieldname:fieldtype" => {FieldName, FieldType}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_field_name_type(FieldNameInfoStr) -> FieldInfo when
    FieldNameInfoStr :: csv_line(),
    FieldInfo :: field_info().
get_field_name_type(FieldNameInfoStr) ->
    [FieldNameStr | TailFieldTypeStr] = string:tokens(FieldNameInfoStr, ?NAME_TYPE_SEPARATOR),
    FieldName = list_to_atom(FieldNameStr),
    FieldType =
        case TailFieldTypeStr of
            [] ->
                string;
            [FieldTypeStr | _RestFieldTypeStrs] ->
                list_to_atom(FieldTypeStr)
        end,
    {FieldName, FieldType}.