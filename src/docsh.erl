-module(docsh).

-export([main/1]).

-export_type([external/0,
              internal/0]).

-import(docsh_lib, [print/2, print/3]).

%% External documentation format.
%% Right now the only supported format is docsh_elixir_docs_v1 which
%% (TODO) aims to be compatible with Elixir at some point in the future.
-type external() :: any().

%% Internal documentation format.
%% All `docsh_reader` modules convert to this format from their input.
%% All `docsh_writer` modules convert from this format to desired output.
-type internal() :: [{atom(), any()}].

%%
%% Escript API
%%

main(["transform", BEAMFile]) ->
    'try'(fun () -> process_beam(BEAMFile) end);
main(["transform", BEAMFile, "to", NewBEAMFile]) ->
    'try'(fun () -> {ok, NewBEAM} = process_beam(BEAMFile),
                    ok = file:write_file(NewBEAMFile, NewBEAM) end);
main(["diff", BEAM1, BEAM2]) ->
    'try'(fun () -> print("~p~n", [docsh_lib:beam_diff(BEAM1, BEAM2)]) end);
main(_) ->
    usage(),
    erlang:halt(1).

process_beam(BEAMFile) ->
    case {has_exdc(BEAMFile),
          get_debug_info(BEAMFile),
          get_source_file(BEAMFile)}
    of
        {true, _, _} ->
            error("ExDc already present", [BEAMFile]);
        {false, {ok, Abst}, _} ->
            rebuild(BEAMFile, [exdc({abst, Abst})]);
        %% TODO exdc from source file
        %{false, _, {ok, File}} ->
        %    rebuild(BEAMFile, [exdc({source, File})]);
        _ ->
            error("neither debug_info nor .erl available", [BEAMFile])
    end.

%%
%% Helpers
%%

'try'(F) ->
    try F()
    catch
        _:R -> print(standard_error, "~s: ~s~n", [progname(), R]),
               print(standard_error, "~p~n", [erlang:get_stacktrace()]),
               erlang:halt(2)
    end.

usage() ->
    print(standard_error,
          "usage: ~s~n", [progname()]).

progname() ->
    filename:basename(hd(init:get_plain_arguments())).

has_exdc(BEAMFile) ->
    {ok, _, Chunks} = beam_lib:all_chunks(BEAMFile),
    case catch ([ throw(true) || {"ExDc", _} <- Chunks ]) of
        true -> true;
        _    -> false
    end.

-spec get_debug_info(file:filename()) -> binary().
get_debug_info(BEAMFile) ->
    case beam_lib:chunks(BEAMFile, ["Abst"]) of
        {ok, {_Module, [{"Abst", Abst}]}} -> {ok, Abst};
        _ -> false
    end.

-spec get_source_file(file:filename()) -> file:filename().
get_source_file(BEAMFile) ->
    try get_source((modname(BEAMFile)):module_info(compile)) of
        File when is_list(File) ->
            case filelib:is_regular(File) of
                true -> {ok, File};
                _ -> false
            end
    catch _:_ -> false end.

-spec modname(string()) -> module().
modname(BEAMFile) ->
    Base = filename:basename(BEAMFile, ".beam"),
    list_to_atom(Base).

get_source(CompileInfo) ->
    {source, File} = lists:keyfind(source, 1, CompileInfo),
    File.

exdc({abst, BAbst}) ->
    {raw_abstract_v1, Abst} = binary_to_term(BAbst),
    FromMods = [docsh_edoc, docsh_syntax],
    ToMod = docsh_elixir_docs_v1,
    ExDc = docsh_lib:convert(FromMods, ToMod, Abst),
    {"ExDc", term_to_binary(ExDc)}.
%% TODO exdc from source file
%exdc({source, File})) ->

rebuild(BEAMFile, NewChunks) ->
    {ok, _, OldChunks} = beam_lib:all_chunks(BEAMFile),
    {ok, _NewBEAM} = beam_lib:build_module(OldChunks ++ NewChunks).
