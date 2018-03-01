-module(docsh_lib).

-export([beam_diff/2,
         convert/3,
         debug/3,
         format_error/1,
         get/2, get/3,
         get_debug_info/1,
         get_source_file/1,
         has_exdc/1,
         is_module_available/1,
         print/2, print/3,
         process_beam/1]).

-export_type([compiled_module/0]).

-type k() :: any().
-type v() :: any().

%% A `compiled_module()' is a subtype of `beam_lib:beam()' representing the in-memory
%% assembled chunks of a module.
%% You can get one e.g. by reading a .beam file directly or by calling `beam_lib:build_module/1'.
-type compiled_module() :: binary().

-spec convert(Readers, Writer, Beam) -> docsh:external() when
      Readers :: [module()],
      Writer :: module(),
      Beam :: docsh_beam:t().
convert(Readers, Writer, Beam) ->
    InternalDocs = lists:flatmap(fun convert_one/1, [ {R, Beam} || R <- Readers ]),
    Merged = docsh_internal:merge(InternalDocs),
    Writer:from_internal(Merged).

convert_one({Reader, Mod}) ->
    case Reader:to_internal(Mod) of
        {error, R} ->
            %% TODO: this is a *_lib module - shouldn't we bubble it up?
            print(standard_error, "~s\n", [format_error(R)]),
            [];
        {ok, InternalDoc} ->
            [InternalDoc]
    end.

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

-spec process_beam(beam_lib:beam()) -> {ok, binary(), [Warning]} when
      Warning :: no_debug_info | no_src.
process_beam(BEAMFile) ->
    has_exdc(BEAMFile)
        andalso error(exdc_present, [BEAMFile]),
    {ok, Beam} = docsh_beam:from_beam_file(BEAMFile),
    case {docsh_beam:abst(Beam), docsh_beam:source_file(Beam)} of
        {false, false} ->
            error(no_debug_info_no_src, [BEAMFile]);
        {_, false} ->
            {ok, Bin} = add_chunks(BEAMFile, [exdc(Beam)]),
            {ok, Bin, [no_src]};
        {false, _} ->
            {ok, Bin} = add_chunks(BEAMFile, [exdc(Beam)]),
            {ok, Bin, [no_debug_info]};
        _ ->
            {ok, Bin} = add_chunks(BEAMFile, [exdc(Beam)]),
            {ok, Bin, []}
    end.

-spec has_exdc(beam_lib:beam()) -> boolean().
has_exdc(BEAMFile) ->
    {ok, _, Chunks} = beam_lib:all_chunks(BEAMFile),
    case catch ([ throw(true) || {"ExDc", _} <- Chunks ]) of
        true -> true;
        _    -> false
    end.

-spec get_debug_info(file:filename()) -> binary().
get_debug_info(BEAMFile) ->
    case beam_lib:chunks(BEAMFile, ["Abst"]) of
        {ok, {_Module, [{"Abst", <<>>}]}} -> false;
        {ok, {_Module, [{"Abst", Abst}]}} -> {ok, Abst};
        _ -> false
    end.

-spec get_source_file(file:filename()) -> file:filename().
get_source_file(BEAMFile) ->
    lists:foldl(fun check_source_file/2,
                false,
                lists:concat([compile_info_source_file(BEAMFile),
                              guessed_source_file(BEAMFile)])).

check_source_file(_SourceFile, {ok, File}) ->
    {ok, File};
%% This is a special case for Erlang prebuilt .beam files.
%% Calling filelib:is_regular/1 on non-existent files under /net might
%% take ~20 seconds on Mac - we want to avoid it.
check_source_file("/net" ++ _, false) ->
    false;
check_source_file(SourceFile, false) ->
    case filelib:is_regular(SourceFile) of
        true -> {ok, SourceFile};
        false -> false
    end.

compile_info_source_file(BEAMFile) ->
    {ok, {_, [{_, CInf}]}} = beam_lib:chunks(BEAMFile, ["CInf"]),
    {source, File} = lists:keyfind(source, 1, erlang:binary_to_term(CInf)),
    [File].

-spec guessed_source_file(file:filename() | compiled_module()) -> [file:filename()].
guessed_source_file(CompiledModule) when is_binary(CompiledModule) ->
    %% Can't guess source file for an in-memory compiled module,
    %% as it might've not been read from any on-disk .beam file.
    [];
guessed_source_file(BEAMFile) ->
    File1 = filename:join(filename:dirname(BEAMFile), "../src"),
    File2 = filename:basename(BEAMFile, ".beam") ++ ".erl",
    [filename:join(File1, File2)].

-spec exdc(docsh_beam:t()) -> {string(), binary()}.
exdc(Beam) ->
    FromMods = available_readers(Beam),
    FromMods == []
        andalso error(no_readers_available),
    ToMod = docsh_elixir_docs_v1,
    ExDc = convert(FromMods, ToMod, Beam),
    {"ExDc", term_to_binary(ExDc, [compressed])}.

add_chunks(BEAMFile, NewChunks) ->
    {ok, _, OldChunks} = beam_lib:all_chunks(BEAMFile),
    {ok, _NewBEAM} = beam_lib:build_module(OldChunks ++ NewChunks).

-spec format_error(any()) -> iolist().
format_error({no_debug_info, Mod}) ->
    io_lib:format("Abstract code for ~s is not available.\n", [Mod]);
format_error({no_src, Mod}) ->
    io_lib:format("Source file for ~s is not available.\n", [Mod]);
format_error(exdc_present) ->
    <<"ExDc chunk already present">>;
format_error(no_debug_info_no_src) ->
    <<"neither debug_info nor .erl available">>;
format_error(Reason) when is_list(Reason);
                          is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    Stacktrace = erlang:get_stacktrace(),
    io_lib:format("docsh error: ~p~n~p~n", [Reason, Stacktrace]).

-spec available_readers(docsh_beam:t()) -> [docsh_reader:t()].
available_readers(Beam) ->
    docsh_edoc:available(Beam) ++
    docsh_syntax:available(Beam).

-spec is_module_available(module()) -> boolean().
is_module_available(Mod) ->
    try
        Mod:module_info(),
        true
    catch
        _:undef -> false
    end.
