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
-export([traverse_merge_files/2,
    traverse_files/1,
    traverse_files/2,
    parse_file/1,
    parse_file/2]).

-define(FILE_EXTENSION, ".csv").
-define(NAME_TYPE_SEPARATOR, ":").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder and merge.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_merge_files(FolderPath, RowFun) -> MapFromFiles when
    FolderPath :: file:name(),
    RowFun :: function(),
    MapFromFiles :: map().
traverse_merge_files(FolderPath, RowFun) ->
    {ok, FileNameList} = file:list_dir(FolderPath),
    lists:foldl(
        fun(FileName, AccMap) ->
            case filename:extension(FileName) == ?FILE_EXTENSION of
                true ->
                    maps:merge(AccMap, parse_file(filename:join(FolderPath, FileName), RowFun));
                _ ->
                    AccMap
            end
        end,
        #{},
        FileNameList).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder with default
%% row function.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_files(FolderPath) -> MapFromFiles when
    FolderPath :: file:name(),
    MapFromFiles :: map().
traverse_files(FolderPath) ->
    traverse_files(FolderPath, fun default_row_fun/1).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_files(FolderPath, RowFun) -> MapFromFiles when
    FolderPath :: file:name(),
    RowFun :: function(),
    MapFromFiles :: map().
traverse_files(FolderPath, RowFun) ->
    {ok, FileNameList} = file:list_dir(FolderPath),
    lists:foldl(
        fun(FileName, AccMap) ->
            case filename:extension(FileName) == ?FILE_EXTENSION of
                true ->
                    Key = list_to_atom(filename:rootname(FileName)),
                    ValuesMap = parse_file(filename:join(FolderPath, FileName), RowFun),
                    AccMap#{Key => ValuesMap};
                _ ->
                    AccMap
            end
        end,
        #{},
        FileNameList).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs with default row function from file which
%% the number of object configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath) -> MapFromFile when
    FilePath :: file:filename(),
    MapFromFile :: map().
parse_file(FilePath) ->
    {ok, File} = file:open(FilePath, [read]),
    {ok, MapFromFile} = ecsv:process_csv_file_with(File, fun traverse_rows/2, {0, fun default_row_fun/1}),
    ok = file:close(File),
    MapFromFile.

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from file which the number of object
%% configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath, RowFun) -> MapFromFile when
    FilePath :: file:filename(),
    RowFun :: function,
    MapFromFile :: map().
parse_file(FilePath, RowFun) ->
    {ok, File} = file:open(FilePath, [read]),
    {ok, MapFromFile} = ecsv:process_csv_file_with(File, fun traverse_rows/2, {0, RowFun}),
    ok = file:close(File),
    MapFromFile.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads line from csv file
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_rows(NewRowData, State) -> KeyValuesMap when
    NewRowData :: {newline, NewRow} | {eof},
    NewRow :: [term()],
    RowFun :: function(),
    KeyValuesMap :: map(),
    Counter :: non_neg_integer(),
    KeysMap :: map(),
    AccRowValuesMap :: map(),
    State :: {Counter, RowFun, KeysMap, AccRowValuesMap} | {0, RowFun} | {_, _, _, KeyValuesMap}.
traverse_rows({newline, NewRow}, {Counter, RowFun, #{0 := {FieldName, _}} = KeysMap, AccRowValuesMap}) ->
    case traverse_column(NewRow, KeysMap, #{}, 0) of
        invalid_row ->
            {Counter + 1, RowFun, KeysMap, AccRowValuesMap};
        CurRowValuesMap ->
            CurRowKey = maps:get(FieldName, CurRowValuesMap),
            CurRowValue = RowFun(CurRowValuesMap),
            {Counter + 1, RowFun, KeysMap, AccRowValuesMap#{CurRowKey => CurRowValue}}
    end;
traverse_rows({newline, NewRow}, {0, RowFun}) ->
    KeysMap = gen_keysmap(NewRow, #{}, 0),
    {1, RowFun, KeysMap, #{}};
traverse_rows({eof}, {_, _, _, RowValuesMap}) ->
    RowValuesMap.

%%--------------------------------------------------------------------
%% @doc
%% Generates keys map from first row
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_keysmap(NewRow, KeysMap, Pos) -> FinalKeysMap when
    NewRow :: [term()],
    KeysMap :: map(),
    Pos :: non_neg_integer(),
    FinalKeysMap :: map().
gen_keysmap([], KeysMap, _) ->
    KeysMap;
gen_keysmap([RawKeyStr | Tail], KeysMap, Pos) ->
    FieldInfo = get_field_name_type(RawKeyStr),
    gen_keysmap(Tail, KeysMap#{Pos => FieldInfo}, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Generates values map from rest of columns of current row.
%%
%% Supported types:
%% integer, float, atom, string, and other terms
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_column(RowData, KeysMap, ValuesMap, Pos) -> FinalValuesMap when
    RowData :: [term()],
    KeysMap :: map(),
    ValuesMap :: map(),
    Pos :: non_neg_integer(),
    FinalValuesMap :: map() | invalid_row.
traverse_column([[] | _], _, _, 0) ->
    invalid_row;
traverse_column([], _, ValuesMap, _) ->
    ValuesMap;
traverse_column([RawValue | TailValues], KeysMap, ValuesMap, Pos) ->
    {FieldName, FieldType} = maps:get(Pos, KeysMap),
    Value =
        try
            case RawValue of
                [] ->
                    RawValue;
                _ ->
                    case FieldType of
                        atom ->
                            list_to_atom(RawValue);
                        integer ->
                            list_to_integer(RawValue);
                        float ->
                            list_to_float(RawValue);
                        term ->
                            {ok, Tokens, _} = erl_scan:string(RawValue),
                            {ok, Term} = erl_parse:parse_term(Tokens),
                            Term;
                        _ ->
                            RawValue
                    end
            end
        catch
            Type:Report ->
                error_logger:error_report(Type, Report),
                RawValue
        end,
    UpdatedValuesMap = ValuesMap#{FieldName => Value},
    traverse_column(TailValues, KeysMap, UpdatedValuesMap, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Default row function which directly return row values map.
%%
%% @end
%%--------------------------------------------------------------------
default_row_fun(RowValuesMap) ->
    RowValuesMap.

%%--------------------------------------------------------------------
%% @doc
%% Get field name and field type from raw field name info string.
%% "fieldname:fieldtype" => {FieldName, FieldType}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_field_name_type(FieldNameInfoStr) -> {FieldName, FieldType} when
    FieldNameInfoStr :: string(),
    FieldName :: atom(),
    FieldType :: atom().
get_field_name_type(FieldNameInfoStr) ->
    [FieldNameStr | TailFieldTypeStr] = string:tokens(FieldNameInfoStr, ?NAME_TYPE_SEPARATOR),
    FieldName = list_to_atom(FieldNameStr),
    FieldType =
        case TailFieldTypeStr of
            [] ->
                string;
            [FieldTypeStr | _] ->
                list_to_atom(FieldTypeStr)
        end,
    {FieldName, FieldType}.