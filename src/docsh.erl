-module(docsh).

-export([main/1]).

-export_type([internal/0]).

-import(docsh_lib, [print/2, print/3]).

%% Internal documentation format.
%% All `docsh_reader` modules convert to this format from their input.
%% All `docsh_writer` modules convert from this format to desired output.
-type internal() :: [{atom(), any()}].

%% External documentation format.
%% Right now the only supported format is docsh_elixir_docs_v1 which aims
%% to be compatible with Elixir at some point in the future.
-type external() :: any().

%%
%% Escript API
%%

main(["beam" | BEAMFile]) -> 'try'(fun () -> process_beam(BEAMFile) end);
main(_) ->
    usage(),
    init:stop(1).

process_beam(BEAMFile) ->
    case {has_debug_info(BEAMFile),
          has_source_available(BEAMFile)}
    of
        {{true, Abst}, _} ->
            rebuild(BEAMFile, [exdc({abst, Abst})]);
        {_, {true, File}} ->
            rebuild(BEAMFile, [exdc({source, File})]);
        _ -> error("neither debug_info nor .erl available", [BEAMFile])
    end.

%%
%% Helpers
%%

'try'(F) ->
    try F()
    catch error:R -> print(standard_error, "~s: ~s~n", [progname(), R]) end.

usage() ->
    print(standard_error,
          "~s: usage~n", [progname()]).

progname() ->
    filename:basename(hd(init:get_plain_arguments())).

has_debug_info(BEAMFile) ->
    case beam_lib:chunks(BEAMFile, ["Abst"]) of
        {ok, {_Module, [{"Abst", Abst}]}} -> {true, Abst};
        _ -> false
    end.

has_source_available(BEAMFile) ->
    try get_source((modname(BEAMFile)):module_info(compile)) of
        File when is_list(File) ->
            case filelib:is_regular(File) of
                true -> {true, File};
                _ -> false
            end
    catch _:_ -> false end.

modname(BEAMFile) ->
    Base = filename:basename(BEAMFile, ".beam"),
    list_to_atom(Base).

get_source(CompileInfo) ->
    {source, File} = lists:keyfind(source, 1, CompileInfo),
    File.

exdc({abst, Abst}) ->
    FromMods = [docsh_edoc, docsh_syntax],
    ToMod = docsh_elixir_docs_v1,
    ExDc = docsh_lib:convert(FromMods, ToMod, binary_to_term(Abst)),
    {"ExDc", term_to_binary(ExDc)}.
%exdc({source, File})) ->

rebuild(BEAMFile, NewChunks) ->
    {ok, _, OldChunks} = beam_lib:all_chunks(BEAMFile),
    check_for_exdc(OldChunks),
    beam_lib:build_module(OldChunks ++ NewChunks).

check_for_exdc(Chunks) ->
    [ error("ExDc already present", [Chunks]) || {"ExDc", _} <- Chunks ].
