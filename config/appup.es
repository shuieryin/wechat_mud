#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname generate_appup verbose

%% noinspection ErlangUnusedFunction
main(Args) ->
    try
        [Option | TailArgs] = Args,
        case list_to_atom(Option) of
            gen_appup ->
                [AppName, OldVsn] = TailArgs,
                start(AppName, OldVsn);
            rollback_vsn ->
                [AppName, OldVsn] = TailArgs,
                update_version(AppName, OldVsn)
        end
    catch
        _:Reason ->
            io:format("~tp~nerror", [Reason]),
            halt(1)
    end.

start(AppName, OldVsn) ->
    NewVsn = increase_vsn(OldVsn, 3, 1), %% will not modify version number in rebar.config and [app_name].app.src

    %% -------------------------get existing instructions - start-------------------------
    OldAppupPath = "ebin/" ++ AppName ++ ".appup",
    ExistingInstructions =
        case file:consult(OldAppupPath) of
            {ok, [{OldVsn, [{_, SrcInstructions}], [{_, []}]}]} ->
                update_existing_instruction_version(SrcInstructions, OldVsn, NewVsn, []);
            _ ->
                []
        end,
    %% -------------------------get existing instructions - end---------------------------

    %% -------------------------get changed csv files - start---------------------------
    ListModifiedPrivFilesCommand = binary_to_list(<<"git diff --name-only HEAD~0 --diff-filter=M | grep -E 'priv/.*\.csv'">>),
    ListAddedPrivFilesCommand = binary_to_list(<<"git ls-files --others --exclude-standard | grep -E 'priv/.*\.cv'; git diff --name-only HEAD~0 --diff-filter=A | grep -E 'priv/.*\.csv'">>),
    ListDeletedPrivFilesCommand = binary_to_list(<<"git diff --name-only HEAD~0 --diff-filter=D | grep -E 'priv/.*\.csv'">>),

    ModifiedPrivFiles = string:tokens(os:cmd(ListModifiedPrivFilesCommand), "\n"),
    AddedPrivFiles = string:tokens(os:cmd(ListAddedPrivFilesCommand), "\n"),
    DeletedPrivFiles = string:tokens(os:cmd(ListDeletedPrivFilesCommand), "\n"),
    %% -------------------------get changed csv files - end-----------------------------

    %% -------------------------generate new instructions - start-------------------------
    BeamFolder = os:cmd("rebar3 path --app " ++ AppName),
    ModifiedFiles = string:tokens(os:cmd("git diff --name-only HEAD~0 --diff-filter=M | grep -E 'src.*\.erl'"), "\n"),
    {ModifiedInstructions, {UpdatedModifiedPrivFiles, UpdatedAddedPrivFiles, UpdatedDeletedPrivFiles}} = generate_modified_instruction(modified, ModifiedFiles, OldVsn, NewVsn, BeamFolder, [], {ModifiedPrivFiles, AddedPrivFiles, DeletedPrivFiles}),

    ModifiedInstructionsWithModPriv = generate_instruction_for_priv(OldVsn, NewVsn, modified, BeamFolder, ModifiedInstructions, UpdatedModifiedPrivFiles),
    ModifiedInstructionsWithAddPriv = generate_instruction_for_priv(OldVsn, NewVsn, added, BeamFolder, ModifiedInstructionsWithModPriv, UpdatedAddedPrivFiles),
    UpdatedModifiedInstructions = generate_instruction_for_priv(OldVsn, NewVsn, deleted, BeamFolder, ModifiedInstructionsWithAddPriv, UpdatedDeletedPrivFiles),

    DeletedFiles = string:tokens(os:cmd("git diff --name-only HEAD~0 --diff-filter=D | grep -E 'src.*\.erl'"), "\n"),
    DeletedModifiedInstructions = generate_added_deleted_instruction(delete_module, DeletedFiles, UpdatedModifiedInstructions),

    AddedFiles = string:tokens(os:cmd("git ls-files --others --exclude-standard | grep -E 'src.*\.erl'; git diff --name-only HEAD~0 --diff-filter=A | grep -E 'src.*\.erl'"), "\n"),
    AddedDeletedModifiedInstructions = generate_added_deleted_instruction(add_module, AddedFiles, DeletedModifiedInstructions),
    %% -------------------------generate new instructions - end---------------------------

    CodeChangesInstructions = ukeymerge(2, AddedDeletedModifiedInstructions, ExistingInstructions),

    case CodeChangesInstructions of
        [] ->
            io:format("no_change");
        _ ->
            gen_appup(AppName, OldVsn, NewVsn, OldAppupPath, CodeChangesInstructions)
    end.

generate_instruction_for_priv(OldVsn, NewVsn, FileType, BeamFolder, AccInstructions, PrivFilePahts) ->
    lists:foldl(
        fun(FilePath, AccModifiedInstructions) ->
            [_, CurModNameStr | _] = filename:split(FilePath),
            CurModName = list_to_atom(CurModNameStr),
            UpdatedInstruction =
                case lists:keyfind(CurModName, 2, AccModifiedInstructions) of
                    false ->
                        BeamFilePath = filename:join(BeamFolder, CurModNameStr ++ ".beam"),
                        case filelib:is_file(BeamFilePath) of
                            true ->
                                NewExtra =
                                    case FileType of
                                        modified ->
                                            {[FilePath], [], []};
                                        added ->
                                            {[], [FilePath], []};
                                        deleted ->
                                            {[], [], [list_to_atom(filename:basename(filename:rootname(FilePath)))]}
                                    end,
                                {update, CurModName, {advanced, {OldVsn, NewVsn, NewExtra}}};
                            false ->
                                throw("[" ++ FilePath ++ "] cannot be processed because module [" ++ CurModNameStr ++ "] does not exist.")
                        end;
                    {update, CurModName, {advanced, {OldVsn, NewVsn, CurExtra}}} ->
                        {AccModPaths, AccAddPaths, AccDelPaths} = CurExtra,
                        UpdatedCurExtra
                            = case FileType of
                                  modified ->
                                      {[FilePath | AccModPaths], AccAddPaths, AccDelPaths};
                                  added ->
                                      {AccModPaths, [FilePath | AccAddPaths], AccDelPaths};
                                  deleted ->
                                      {AccModPaths, AccAddPaths, [list_to_atom(filename:basename(filename:rootname(FilePath))) | AccDelPaths]}
                              end,
                        {update, CurModName, {advanced, {OldVsn, NewVsn, UpdatedCurExtra}}}
                end,
            lists:keyreplace(CurModName, 2, AccModifiedInstructions, UpdatedInstruction)
        end, AccInstructions, PrivFilePahts).

gen_appup(AppName, OldVsn, NewVsn, OldAppupPath, FinalInstructions) ->
    update_version(AppName, NewVsn),
    AppupContent = {NewVsn,
        [{OldVsn, FinalInstructions}],
        [{OldVsn, []}]},
    os:cmd("mkdir -p ebin"),
    AppupContentBin = io_lib:format("~tp.", [AppupContent]),
    file:write_file(OldAppupPath, AppupContentBin),
    file:write_file("config/" ++ AppName ++ ".appup", AppupContentBin),
    io:format("~tp", [NewVsn]).

generate_added_deleted_instruction(Status, [SrcFilePath | Tail], AccInstructions) when add_module == Status orelse delete_module == Status ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    Instruction = {Status, ModName},
    generate_added_deleted_instruction(Status, Tail, [Instruction | AccInstructions]);
generate_added_deleted_instruction(_, [], InstructionList) ->
    InstructionList.

generate_modified_instruction(modified, [SrcFilePath | Tail], OldVsn, NewVsn, BeamFolder, AccInstructions, PrivChangedFiles) ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    ModFileName = ModNameStr ++ ".beam",
    BeamFilePath = filename:join(BeamFolder, ModFileName),
    {Instruction, UpdatedPrivChangedFiles} =
        case file:read_file(BeamFilePath) of
            {ok, Beam} ->
                {ok, {_, [{exports, Exports}, {attributes, Attributes}]}} = beam_lib:chunks(Beam, [exports, attributes]),
                Behaviour = proplists:get_value(behaviour, Attributes, []),
                case lists:member(supervisor, Behaviour) of
                    true ->
                        {
                            {update, ModName, supervisor},
                            PrivChangedFiles
                        };
                    _ ->
                        case lists:member({code_change, 3}, Exports) orelse lists:member({code_change, 4}, Exports) of
                            true ->
                                {AccModifiedFiles, AccAddedFiles, AccDeletedFiles} = PrivChangedFiles,

                                {ModifiedFilesExtra, RestModifiedFiles} = filter_priv_files(AccModifiedFiles, ModNameStr),
                                {AddedFilesExtra, RestAddedFiles} = filter_priv_files(AccAddedFiles, ModNameStr),
                                {DeletedFilesExtra, RestDeletedFiles} = filter_priv_files(AccDeletedFiles, ModNameStr),

                                {
                                    {update, ModName, {advanced, {OldVsn, NewVsn, {ModifiedFilesExtra, AddedFilesExtra, DeletedFilesExtra}}}},
                                    {RestModifiedFiles, RestAddedFiles, RestDeletedFiles}
                                };
                            _ ->
                                {
                                    {load_module, ModName},
                                    PrivChangedFiles
                                }
                        end
                end;
            _ ->
                io:format("Could not read ~s\n", [BeamFilePath])
        end,
    generate_modified_instruction(modified, Tail, OldVsn, NewVsn, BeamFolder, [Instruction | AccInstructions], UpdatedPrivChangedFiles);
generate_modified_instruction(_, [], _, _, _, InstructionList, PrivChangedFiles) ->
    {InstructionList, PrivChangedFiles}.

filter_priv_files(FilePaths, ModNameStr) ->
    lists:foldl(
        fun(FilePath, {AccFilePaths, AccOriFilePaths}) ->
            case re:run(FilePath, "^priv/" ++ ModNameStr ++ ".*") of
                {match, _Captured} ->
                    {
                        [FilePath | AccFilePaths],
                        lists:delete(FilePath, AccOriFilePaths)
                    };
                nomatch ->
                    {AccFilePaths, AccOriFilePaths}
            end
        end, {[], FilePaths}, FilePaths).

update_version(AppName, TargetVsn) ->
    RelVsnMarker = "release-version-marker",
    os:cmd("sed -i.bak 's/\".*\" %% " ++ RelVsnMarker ++ "/\"" ++ TargetVsn ++ "\" %% " ++ RelVsnMarker ++ "/1' src/" ++ AppName ++ ".app.src  ;\
        sed -i.bak 's/\".*\" %% " ++ RelVsnMarker ++ "/\"" ++ TargetVsn ++ "\" %% " ++ RelVsnMarker ++ "/1' rebar.config  ;\
        rm -f rebar.config.bak  ;\
        rm -f src/" ++ AppName ++ ".app.src.bak").

increase_vsn(SourceVersion, VersionDepth, Increment) ->
    string:join(increase_vsn(string:tokens(SourceVersion, "."), VersionDepth, Increment, 1, []), ".").

increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    UpdatedVersionNum =
        case CurDepth =:= VersionDepth of
            true ->
                integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment);
            _ ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]);
increase_vsn([], _, _, _, AccVersion) ->
    lists:reverse(AccVersion).

update_existing_instruction_version([{update, ModName, {advanced, {_, _, []}}} | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [{update, ModName, {advanced, {OldVsn, NewVsn, []}}} | AccResult]);
update_existing_instruction_version([Other | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [Other | AccResult]);
update_existing_instruction_version([], _, _, AccResult) ->
    AccResult.

ukeymerge(ElemPos, SrcList, MergeList) ->
    MergeMap = proplist_to_map(ElemPos, MergeList, #{}),
    FinalMap = proplist_to_map(ElemPos, SrcList, MergeMap),
    maps:values(FinalMap).

proplist_to_map(ElemPos, [Value | Tail], AccMap) ->
    Key = erlang:element(ElemPos, Value),
    proplist_to_map(ElemPos, Tail, AccMap#{Key => Value});
proplist_to_map(_, [], AccMap) ->
    AccMap.