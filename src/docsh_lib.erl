-module(docsh_lib).

-export([beam_diff/2,
         convert/3,
         debug/3,
         format_error/1,
         get/2, get/3,
         get_abstract_code/1,
         get_docs/1,
         get_source_file/1,
         group_by/2,
         has_docs/1,
         is_module_available/1,
         make_docs/1,
         print/2, print/3,
         stick_module/1,
         unstick_module/1]).

-export([compile_info_source_file/1,
         guessed_source_file/1]).

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
        {error, _R, _Stacktrace} = Error ->
            %% TODO: this is a *_lib module - shouldn't we bubble it up?
            print(standard_error, "~s\n", [format_error(Error)]),
            [];
        {ok, InternalDoc} ->
            [InternalDoc]
    end.

%% Error if Key is not found!
-spec get(k(), [{k(), v()}]) -> v().
get(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        false -> erlang:error(not_found, [Key, Props]);
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

-spec has_docs(beam_lib:beam()) -> boolean().
has_docs(BEAMFile) ->
    {ok, _, Chunks} = beam_lib:all_chunks(BEAMFile),
    case catch ([ throw(true) || {"Docs", _} <- Chunks ]) of
        true -> true;
        _    -> false
    end.

-spec get_abstract_code(file:filename()) -> docsh_beam:debug_info() | false.
get_abstract_code(BEAMFile) ->
    case beam_lib:chunks(BEAMFile, [debug_info]) of
        {ok, {_Module, [{debug_info, DbgiV1}]}} ->
            {ok, debug_info_v1(DbgiV1)};
        _ ->
            case beam_lib:chunks(BEAMFile, [abstract_code]) of
                {ok, {_Module, [{abstract_code, RawAbstV1}]}} when is_tuple(RawAbstV1) ->
                    {ok, raw_abstract_v1(RawAbstV1)};
                _ ->
                    false
            end
    end.

raw_abstract_v1({raw_abstract_v1, Forms}) ->
    Forms.

debug_info_v1({debug_info_v1, _, {Forms, _CompileInfo}}) ->
    Forms.

-spec get_source_file(file:filename()) -> {ok, file:filename()} | false.
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

-spec compile_info_source_file(file:filename()) -> [file:filename()].
compile_info_source_file(BEAMFile) ->
    case beam_lib:chunks(BEAMFile, ["CInf"]) of
        {ok, {_, [{_, CInf}]}} ->
            case lists:keyfind(source, 1, erlang:binary_to_term(CInf)) of
                {source, File} -> [File];
                false -> []
            end;
        _ ->
            []
    end.

-spec guessed_source_file(file:filename() | compiled_module()) -> [file:filename()].
guessed_source_file(CompiledModule) when is_binary(CompiledModule) ->
    %% Can't guess source file for an in-memory compiled module,
    %% as it might've not been read from any on-disk .beam file.
    [];
guessed_source_file(BEAMFile) ->
    EbinPrefix = ebin_prefix(BEAMFile),
    SrcSuffix = src_suffix(BEAMFile),
    [filename:join(EbinPrefix ++ SrcSuffix)].

ebin_prefix(BEAMFile) ->
    Components = string:tokens(BEAMFile, "/"),
    case BEAMFile of
        "/" ++ _ -> ["/"];
        _ -> [""]
    end ++ lists:takewhile(fun not_ebin/1, Components).

not_ebin("ebin") -> false;
not_ebin(_) -> true.

src_suffix(BEAMFile) ->
    [CInfSourceFile] = compile_info_source_file(BEAMFile),
    Components = string:tokens(CInfSourceFile, "/"),
    lists:dropwhile(fun not_src/1, Components).

not_src("src") -> false;
not_src(_) -> true.

-spec format_error(any()) -> iolist().
format_error({no_debug_info, Mod}) ->
    io_lib:format("Abstract code for ~s is not available.\n", [Mod]);
format_error({no_src, Mod}) ->
    io_lib:format("Source file for ~s is not available.\n", [Mod]);
format_error(docs_present) ->
    <<"Docs chunk already present">>;
format_error(no_debug_info_no_src) ->
    <<"neither debug_info nor .erl available">>;
format_error(Reason) when is_list(Reason);
                          is_binary(Reason) ->
    Reason;
format_error({error, Reason, Stacktrace}) ->
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

-spec group_by(fun(), list()) -> dict:dict().
group_by(F, L) ->
    lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end,
                dict:new(), [ {F(X), X} || X <- L ]).

-spec get_docs(module()) -> {ok, docsh_format:t()} | {error, any()}.
get_docs(M) ->
    case docsh_beam:from_loaded_module(M) of
        {error, _} = E -> E;
        {ok, B} ->
            MakeDocs = application:get_env(docsh, compile_on_demand, compile_if_missing),
            AvailableDocs = do_get_docs(B),
            dispatch_docs_extraction(B, MakeDocs, AvailableDocs)
    end.

do_get_docs(B) ->
    try
        {ok, docsh_beam:docs(B)}
    catch _:R ->
        {error, R}
    end.

dispatch_docs_extraction(B, never, AvailableDocs) ->
    %% just for troubleshooting
    {ok, [B, never, AvailableDocs]};
dispatch_docs_extraction(_, compile_if_missing, {ok, Docs}) ->
    {ok, Docs};
dispatch_docs_extraction(B, compile_if_missing, {error, _} = Err) ->
    %% TODO: log Reason?
    dispatch_docs_extraction(B, always, Err);
dispatch_docs_extraction(B, always, _) ->
    dispatch_docs_extraction_(B).

dispatch_docs_extraction_(B) ->
    {ok, Docs, Warnings} = make_docs(B),
    [ print("~s", [docsh_lib:format_error({W, docsh_beam:name(B)})]) || W <- Warnings ],
    %% TODO: enable cache at some point
    %cache_docs(B, Docs),
    {ok, Docs}.

-spec make_docs(docsh_beam:t()) -> {ok, docsh_format:t(), [Warning]} when
      Warning :: no_debug_info | no_src.
make_docs(Beam) ->
    BEAMFile = docsh_beam:beam_file(Beam),
    has_docs(BEAMFile)
        andalso erlang:error(docs_present, [BEAMFile]),
    case {docsh_beam:abstract_code(Beam), docsh_beam:source_file(Beam)} of
        {false, false} ->
            erlang:error({no_debug_info_no_src, BEAMFile}, [BEAMFile]);
        {_, false} ->
            {ok, do_make_docs(Beam), [no_src]};
        {false, _} ->
            {ok, do_make_docs(Beam), [no_debug_info]};
        _ ->
            {ok, do_make_docs(Beam), []}
    end.

-spec do_make_docs(docsh_beam:t()) -> {string(), binary()}.
do_make_docs(Beam) ->
    FromMods = get_readers(Beam),
    FromMods == []
        andalso erlang:error(no_readers_available),
    ToMod = application:get_env(docsh, docsh_writer, default_writer()),
    convert(FromMods, ToMod, Beam).

default_writer() ->
    docsh_docs_v1.

get_readers(Beam) ->
    application:get_env(docsh, readers, available_readers(Beam)).

-spec unstick_module(module()) -> any().
unstick_module(Module) -> unstick_module(Module, code:is_sticky(Module)).

unstick_module(Module, true) -> code:unstick_mod(Module);
unstick_module(_,_) -> false.

-spec stick_module(module()) -> any().
stick_module(Module) -> stick_module(Module, code:is_sticky(Module)).

stick_module(Module, false) -> code:stick_mod(Module);
stick_module(_,_) -> false.
