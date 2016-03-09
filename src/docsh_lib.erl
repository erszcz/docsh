-module(docsh_lib).

-export([beam_diff/2,
         convert/3,
         get/2, get/3,
         debug/3,
         print/2, print/3,
         process_beam/1]).

-type k() :: any().
-type v() :: any().

-spec convert(FromMods, ToMod, AST) -> docsh:external() when
      FromMods :: [module()],
      ToMod :: module(),
      AST :: [erl_parse:abstract_form()].
convert(FromMods, ToMod, AST) ->
    Internal = docsh_internal:merge([ FromMod:to_internal(file(AST))
                                      || FromMod <- FromMods ]),
    ToMod:from_internal(Internal).

%% Error if Key is not found!
-spec get(k(), [{k(), v()}]) -> v().
get(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        false -> error(not_found, [Key, Props]);
        {Key, Val} -> Val
    end.

-spec get(k(), [{k(), v()}], v()) -> v().
get(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        false -> Default;
        {Key, Val} -> Val
    end.

-spec print(io:format(), [term()]) -> ok.
print(Fmt, Args) ->
    print(standard_io, Fmt, Args).

-spec print(io:device(), io:format(), [term()]) -> ok.
print(Handle, Fmt, Args) ->
    io:format(Handle, Fmt, Args).

-spec debug(atom(), io:format(), [term()] | fun(() -> [term()])) -> ok.
debug(Tag, Fmt, Args) ->
    case os:getenv("DOCSH_DEBUG") of
        false -> ok;
        Tags -> debug_matching(string:tokens(Tags, ","),
                               atom_to_list(Tag), Fmt, Args)
    end.

debug_matching(Tags, Tag, Fmt, Args) ->
    case lists:member(Tag, Tags) of
        false -> ok;
        true -> print(Fmt, if
                               is_function(Args, 0) -> Args();
                               is_list(Args) -> Args
                           end)
    end.

%% @doc Find file name in an Erlang module abstract syntax tree.
-spec file([erl_parse:abstract_form()]) -> string().
file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.

%% @doc Show the difference between chunk sets of two modules.
-spec beam_diff(BEAM, BEAM) -> [{BEAM, module(), list()}] when
      BEAM :: beam_lib:beam().
beam_diff(BEAM1, BEAM2) ->
    {ok, Name1, Chunks1} = beam_lib:all_chunks(BEAM1),
    {ok, Name2, Chunks2} = beam_lib:all_chunks(BEAM2),
    Keys1 = [ K || {K, _} <- Chunks1 ],
    Keys2 = [ K || {K, _} <- Chunks2 ],
    [{BEAM1, Name1, Keys1 -- Keys2},
     {BEAM2, Name2, Keys2 -- Keys1}].

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
    ExDc = convert(FromMods, ToMod, Abst),
    {"ExDc", term_to_binary(ExDc, [compressed])}.
%% TODO exdc from source file
%exdc({source, File})) ->

rebuild(BEAMFile, NewChunks) ->
    {ok, _, OldChunks} = beam_lib:all_chunks(BEAMFile),
    {ok, _NewBEAM} = beam_lib:build_module(OldChunks ++ NewChunks).
