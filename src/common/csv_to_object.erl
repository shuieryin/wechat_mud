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
    traverse_merge_files/2,
    traverse_files/1,
    traverse_files/2,
    parse_file/1,
    parse_file/2
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
-type csv_to_object() :: #{key() => csv_data()}.

-export_type([csv_data/0,
    csv_to_object/0]).

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
    MapFromFiles :: csv_data().
traverse_merge_files(FolderPath, RowFun) ->
    {ok, FileNameList} = file:list_dir(FolderPath),
    lists:foldl(
        fun(FileName, AccMap) ->
            case filename:extension(FileName) == ?FILE_EXTENSION of
                true ->
                    maps:merge(AccMap, parse_file(filename:join(FolderPath, FileName), RowFun));
                false ->
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
    MapFromFiles :: csv_data().
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
    MapFromFiles :: csv_data().
traverse_files(FolderPath, RowFun) ->
    {ok, FileNameList} = file:list_dir(FolderPath),
    lists:foldl(
        fun(FileName, AccMap) ->
            case filename:extension(FileName) == ?FILE_EXTENSION of
                true ->
                    Key = list_to_atom(filename:rootname(FileName)),
                    ValuesMap = parse_file(filename:join(FolderPath, FileName), RowFun),
                    AccMap#{Key => ValuesMap};
                false ->
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
    MapFromFile :: csv_data().
parse_file(FilePath) ->
    parse_file(FilePath, fun default_row_fun/1).

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from file which the number of object
%% configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath, RowFun) -> MapFromFile when
    FilePath :: file:filename(),
    RowFun :: function(),
    MapFromFile :: csv_data().
parse_file(FilePath, RowFun) ->
    {ok, File} = file:open(FilePath, [read]),
    RecordName = list_to_atom(filename:rootname(filename:basename(FilePath))),
    {ok, MapFromFile} = ecsv:process_csv_file_with(File, fun traverse_rows/2, {0, RowFun, RecordName}),
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
-spec traverse_rows(NewRowData, State) -> RowValuesMap when
    NewRowData :: {newline, [csv_line()]} | {eof},
    RowFun :: function(),
    RowValuesMap :: csv_data(),
    Counter :: non_neg_integer(), % generic integer
    FieldInfos :: field_infos(),
    AccRowValuesMap :: RowValuesMap,
    RecordName :: atom(), % generic atom
    State :: {Counter, RowFun, FieldInfos, AccRowValuesMap, RecordName} | {0, RowFun, RecordName} | {_, _, _, RowValuesMap, _}.
traverse_rows({newline, NewRow}, {Counter, RowFun, FieldInfos, AccRowValuesMap, RecordName}) ->
    [_, CurRowKey | _] = CurRowValues = traverse_column(NewRow, FieldInfos, [RecordName]),
    UpdatedAccRowValuesMap =
        case CurRowKey of
            undefined ->
                AccRowValuesMap;
            _ ->
                case RowFun(CurRowValues) of
                    undefined ->
                        AccRowValuesMap;
                    ConvertedCurRowValues ->
                        AccRowValuesMap#{CurRowKey => ConvertedCurRowValues}
                end
        end,
    {Counter + 1, RowFun, FieldInfos, UpdatedAccRowValuesMap, RecordName};
traverse_rows({newline, NewRow}, {0, RowFun, RecordName}) ->
    FieldInfos = gen_fieldinfos(NewRow, []),
    {1, RowFun, FieldInfos, #{}, RecordName};
traverse_rows({eof}, {_, _, _, RowValuesMap, _}) ->
    RowValuesMap.

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
traverse_column([], _, AccValues) ->
    lists:reverse(AccValues);
traverse_column([RawValue | TailValues], [{_, FieldType} | TailFieldInfos], AccValues) ->
    Value =
        try
            case RawValue of
                [] ->
                    undefined;
                _ ->
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
                            {ok, Tokens, _} = erl_scan:string(RawValue),
                            {ok, Term} = erl_parse:parse_term(Tokens),
                            Term;
                        exprs ->
                            {ok, Tokens, _} = erl_scan:string(RawValue),
                            {ok, Exprs} = erl_parse:parse_exprs(Tokens),
                            Exprs;
                        _ ->
                            RawValue
                    end
            end
        catch
            Type:Report ->
                error_logger:error_msg("Type:~p~nReport:~p~n", [Type, Report]),
                RawValue
        end,
    traverse_column(TailValues, TailFieldInfos, [Value | AccValues]).

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
            [FieldTypeStr | _] ->
                list_to_atom(FieldTypeStr)
        end,
    {FieldName, FieldType}.