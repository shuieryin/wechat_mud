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
-export([traverse_files/2,
    parse_file/2]).

-define(FILE_EXTENSION, ".csv").
-define(NAME_TYPE_SEPARATOR, ":").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from files of given folder.
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_files(FolderPath, RowFun) -> ListFromFiles when
    FolderPath :: file:name(),
    RowFun :: function(),
    ListFromFiles :: [term()].
traverse_files(FolderPath, RowFun) ->
    {ok, FileNameList} = file:list_dir(FolderPath),
    lists:flatten([parse_file(filename:join(FolderPath, FileName), RowFun) || FileName <- FileNameList, filename:extension(FileName) == ?FILE_EXTENSION]).


%%--------------------------------------------------------------------
%% @doc
%% Generates object configs from file which the number of object
%% configs equals to the number of rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_file(FilePath, RowFun) -> ListFromFile when
    FilePath :: file:filename(),
    RowFun :: function,
    ListFromFile :: [any()].
parse_file(FilePath, RowFun) ->
    {ok, File} = file:open(FilePath, [read]),
    {ok, ListFromFile} = ecsv:process_csv_file_with(File, fun traverse_rows/2, {0, RowFun}),
    ok = file:close(File),
    ListFromFile.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads line from csv file
%%
%% @end
%%--------------------------------------------------------------------
-spec traverse_rows(NewRowData, State) -> RowValueList when
    NewRowData :: {newline, NewRow} | {eof},
    NewRow :: [term()],
    RowFun :: function(),
    State :: {Counter, RowFun, KeysMap, AccRowValueList} | {0, RowFun} | {_, _, _, RowValueList},
    Counter :: non_neg_integer(),
    KeysMap :: map(),
    AccRowValueList :: [term()],
    RowValueList :: [term()].
traverse_rows({newline, NewRow}, {Counter, RowFun, KeysMap, AccRowValueList}) ->
    CurRowValuesMap = traverse_column(NewRow, KeysMap, #{}, 0),
    CurRowValue = RowFun(CurRowValuesMap),
    {Counter + 1, RowFun, KeysMap, [CurRowValue | AccRowValueList]};
traverse_rows({newline, NewRow}, {0, RowFun}) ->
    KeysMap = gen_keysmap(NewRow, #{}, 0),
    {1, RowFun, KeysMap, []};
traverse_rows({eof}, {_, _, _, RowValueList}) ->
    RowValueList.

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
    gen_keysmap(Tail, KeysMap#{Pos => RawKeyStr}, Pos + 1).

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
    FinalValuesMap :: map().
traverse_column([], _, ValuesMap, _) ->
    ValuesMap;
traverse_column([[] | Tail], KeysMap, ValuesMap, Pos) ->
    traverse_column(Tail, KeysMap, ValuesMap, Pos + 1);
traverse_column([RawValue | TailValues], KeysMap, ValuesMap, Pos) ->
    FieldNameInfoStr = maps:get(Pos, KeysMap),
    [FieldNameStr | TailFieldTypeStr] = string:tokens(FieldNameInfoStr, ?NAME_TYPE_SEPARATOR),
    FieldName = list_to_atom(FieldNameStr),
    FieldType =
        case TailFieldTypeStr of
            [] ->
                string;
            [FieldTypeStr | _] ->
                list_to_atom(FieldTypeStr)
        end,
    Value =
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
        end,
    UpdatedValuesMap =
        case Value of
            undefined ->
                ValuesMap;
            _ ->
                ValuesMap#{FieldName => Value}
        end,
    traverse_column(TailValues, KeysMap, UpdatedValuesMap, Pos + 1).