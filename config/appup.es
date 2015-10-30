#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname generate_appup verbose

start(AppName, OldVsn) ->
    NewVsn = increase_vsn(OldVsn, 3, 1), %% will not modify version number in rebar.config and [app_name].app.src

    OldAppupPath = "ebin/" ++ AppName ++ ".appup",
    ExistingInstructions =
        case file:consult(OldAppupPath) of
            {ok, [{OldVsn, [{_, Result}], [{_, []}]}]} ->
                update_existing_instruction_version(Result, OldVsn, NewVsn, []);
            _ ->
                []
        end,

    %% -------------------------generate appup - start-------------------------
    BeamFolder = os:cmd("rebar3 path --app " ++ AppName),
    ModifiedFiles = string:tokens(os:cmd("git diff --name-only HEAD~1 --diff-filter=M | grep -E 'src.*\.erl'"), "\n"),
    ModifiedInstructions = generate_modified_instruction(modified, ModifiedFiles, OldVsn, NewVsn, BeamFolder, []),

    DeletedFiles = string:tokens(os:cmd("git diff --name-only HEAD~1 --diff-filter=D | grep -E 'src.*\.erl'"), "\n"),
    DeleteModifiedInstructions = generate_added_deleted_instruction(delete_module, DeletedFiles, ModifiedInstructions),

    AddedFiles = string:tokens(os:cmd("git ls-files --others --exclude-standard | grep -E 'src.*\.erl'; git diff --name-only HEAD~1 --diff-filter=A | grep -E 'src.*\.erl'"), "\n"),
    AddedDeleteModifiedInstructions = generate_added_deleted_instruction(add_module, AddedFiles, DeleteModifiedInstructions),
    %% -------------------------generate appup - end---------------------------

    FinalInstructions = lists:ukeymerge(2, AddedDeleteModifiedInstructions, ExistingInstructions),

    case FinalInstructions of
        [] ->
            io:format("no_change");
        _ ->
            update_version(AppName, NewVsn),
            AppupContent = {NewVsn,
                [{OldVsn, FinalInstructions}],
                [{OldVsn, []}]},
            os:cmd("mkdir -p ebin"),
            AppupContentBin = io_lib:format("~tp.", [AppupContent]),
            file:write_file(OldAppupPath, AppupContentBin),
            file:write_file("config/" ++ AppName ++ ".appup", AppupContentBin),
            io:format("~tp", [NewVsn])
    end.

generate_added_deleted_instruction(_, [], InstructionList) ->
    InstructionList;
generate_added_deleted_instruction(Status, [SrcFilePath | Tail], AccInstructions) when add_module == Status orelse delete_module == Status ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    Instruction = {Status, ModName},
    generate_added_deleted_instruction(Status, Tail, [Instruction | AccInstructions]).

generate_modified_instruction(_, [], _, _, _, InstructionList) ->
    InstructionList;
generate_modified_instruction(modified, [SrcFilePath | Tail], OldVsn, NewVsn, BeamFolder, AccInstructions) ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    ModFileName = ModNameStr ++ ".beam",
    BeamFilePath = filename:join(BeamFolder, ModFileName),
    Instruction =
        case file:read_file(BeamFilePath) of
            {ok, Beam} ->
                {ok, {_, [{exports, Exports}, {attributes, Attributes}]}} = beam_lib:chunks(Beam, [exports, attributes]),
                Behaviour = proplists:get_value(behaviour, Attributes, []),
                case lists:member(supervisor, Behaviour) of
                    true ->
                        {update, ModName, supervisor};
                    _ ->
                        case lists:member({code_change, 3}, Exports) orelse lists:member({code_change, 4}, Exports) of
                            true ->
                                {update, ModName, {advanced, {OldVsn, NewVsn, []}}};
                            _ ->
                                {load_module, ModName}
                        end
                end;
            _ ->
                io:format("Could not read ~s\n", [BeamFilePath])
        end,
    generate_modified_instruction(modified, Tail, OldVsn, NewVsn, BeamFolder, [Instruction | AccInstructions]).

update_version(AppName, TargetVsn) ->
    RelVsnMarker = "release-version-marker",
    os:cmd("sed -i.bak 's/\".*\" %% " ++ RelVsnMarker ++ "/\"" ++ TargetVsn ++ "\" %% " ++ RelVsnMarker ++ "/1' src/" ++ AppName ++ ".app.src  ;\
        sed -i.bak 's/\".*\" %% " ++ RelVsnMarker ++ "/\"" ++ TargetVsn ++ "\" %% " ++ RelVsnMarker ++ "/1' rebar.config  ;\
        rm -f rebar.config.bak  ;\
        rm -f src/" ++ AppName ++ ".app.src.bak").

%% noinspection ErlangUnusedFunction
main([AppName, OldVsn]) ->
    try
        start(AppName, OldVsn)
    catch
        _:Reason ->
            io:format("~p~n", [Reason]),
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: [release-name] [version(x.x.x)]\n"),
    halt(1).

increase_vsn(SourceVersion, VersionDepth, Increment) ->
    string:join(increase_vsn(string:tokens(SourceVersion, "."), VersionDepth, Increment, 1, []), ".").
increase_vsn([], _, _, _, AccVersion) ->
    lists:reverse(AccVersion);
increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    UpdatedVersionNum =
        case CurDepth =:= VersionDepth of
            true ->
                integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment);
            _ ->
                CurDepthVersionNumStr
        end,
    increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [UpdatedVersionNum | AccVersion]).

update_existing_instruction_version([], _, _, AccResult) ->
    AccResult;
update_existing_instruction_version([{update, ModName, {advanced, {_, _, []}}} | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [{update, ModName, {advanced, {OldVsn, NewVsn, []}}} | AccResult]);
update_existing_instruction_version([Other | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [Other | AccResult]).