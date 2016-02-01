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

-include("../data_type/player_profile.hrl").

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
    State ::
    {Counter, RowFun, FieldInfos, AccRowValuesMap, RecordName} |
    {0, RowFun, RecordName} |
    {_Counter, _RowFun, _FieldInfos, RowValuesMap, _RecordName}.
traverse_rows({newline, NewRow}, {Counter, RowFun, FieldInfos, AccRowValuesMap, RecordName}) ->
    [_UselessHead, CurRowKey | _RestCurRowValues] =
        CurRowValues = traverse_column(NewRow, FieldInfos, [RecordName]),
    UpdatedAccRowValuesMap =
        case CurRowKey of
            undefined ->
                AccRowValuesMap;
            _CurRowKey ->
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
traverse_rows({eof}, {_Counter, _RowFun, _FieldInfos, RowValuesMap, _RecordName}) ->
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
traverse_column([], _FieldInfos, AccValues) ->
    lists:reverse(AccValues);
traverse_column([RawValue0 | TailValues], [{_Key, FieldType} | TailFieldInfos], AccValues) ->
    RawValue = re:replace(RawValue0, [226, 128, 168], "", [global, {return, list}]), % eliminate new line genereated by Numbers.app
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
                            {FromValueNames, RawValue2} = collect_formula_value_names(list_to_binary(RawValue), "From"),
                            {ToValueNames, RawValue3} = collect_formula_value_names(RawValue2, "To"),

                            {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(RawValue3)),
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
    traverse_column(TailValues, TailFieldInfos, [Value | AccValues]).

%%--------------------------------------------------------------------
%% @doc
%% Collect value names form formula.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_formula_value_names(RawValue, ObjectName) -> {ValueNames, UpdatedRawValue} when
    RawValue :: binary(),
    ObjectName :: string(),
    ValueNames :: [atom()], % generic atom
    UpdatedRawValue :: binary().
collect_formula_value_names(RawValue, ObjectName) ->
    MatchRE = list_to_binary(ObjectName ++ "_([A-z0-9]*)[\s|.]{1}"),
    case re:run(RawValue, MatchRE, [global, {capture, all_but_first, binary}]) of
        {match, Matched} ->
            ReplaceRE = list_to_binary(ObjectName ++ "_"),
            {cm:binaries_to_atoms(lists:flatten(Matched)), re:replace(RawValue, ReplaceRE, <<"">>, [global, {return, binary}])};
        nomatch ->
            {[], RawValue}
    end.

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