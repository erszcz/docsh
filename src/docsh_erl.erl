%% @doc Documentation lookup functions exported in the Erlang shell.

-module(docsh_erl).

-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/1, t/2, t/3]).

-import(docsh_embeddable, [key_to_module/1]).
-import(docsh_lib, [print/2]).

%% Function or type name.
-type name() :: docsh_embeddable:name().

%% @doc When invoked with a module as an argument like `h(lists)',
%% then look up the module documentation.
%% When invoked with a fun as the argument like `h(fun lists:keyfind/3)',
%% then equivalent to `h(M, F, A)' with the the fun's deduced
%% `{M :: module(), F :: name(), A :: arity()}'.
-spec h(fun() | module()) -> ok.
h(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    h(M, F, A);

h(M) when is_atom(M) ->
    lookup(M, [moduledoc]).

%% @doc Look up function documentation. Equivalent to `h(M, F, any)'.
-spec h(module(), name()) -> ok.
h(M, F) -> h(M, F, any).

%% @doc Look up function documentation.
%% If arity is 'any', then all matching functions' docs will be printed.
%% The complete info is function name/arity, spec and description.
%% Use `s/{1,2,3}' to print just the spec.
-spec h(module(), name(), arity() | 'any') -> ok.
h(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [doc, spec]).

%% @doc Look up a function spec.
%% Similar to `h/1' used with a fun, but prints just the spec.
-spec s(fun()) -> ok.
s(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    s(M, F, A).

%% @doc Look up function spec. Equivalent to `s(M, F, any)'.
-spec s(module(), name()) -> ok.
s(M, F) -> s(M, F, any).

%% @doc Look up function spec.
%% If arity is 'any' print specs for all matching functions.
-spec s(module(), name(), arity() | 'any') -> ok.
s(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [spec]).

%% @doc Look up all types defined in module `M'.
-spec t(module()) -> ok.
t(M) when is_atom(M) ->
    lookup(M, [type]).

%% @doc Look up type definition. Equivalent to `t(M, T, any)'.
-spec t(module(), name()) -> ok.
t(M, T) -> t(M, T, any).

%% @doc Look up type definition.
%% If arity is 'any' print definitions for all matching types.
-spec t(module(), name(), arity() | 'any') -> ok.
t(M, T, Arity) when is_atom(M), is_atom(T),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, T, Arity}, [type]).

-spec lookup(Key, Items) -> 'ok' when
      Key :: docsh_embeddable:key(),
      Items :: [docsh_embeddable:item_kind()].
lookup(Key, Items) ->
    case get_beam(key_to_module(Key)) of
        {error, R} -> error(R, Key);
        {ok, _Beam} ->
            docsh_embeddable:lookup(Key, Items)
    end.

get_beam(M) ->
    case docsh_beam:from_loadable_module(M) of
        {error, _} = E -> E;
        {ok, B} ->
            case docsh_lib:has_exdc(docsh_beam:beam_file(B)) of
                true -> {ok, B};
                false ->
                    {ok, NewB} = cached_or_rebuilt(B, ensure_cache_dir()),
                    reload(NewB),
                    %% M reloaded from cache will have its .beam_file pointing at the cache.
                    %% This will cause the source resolution mechanism to fail.
                    %% We have to fix that for EDoc availability check to work properly.
                    get_beam(M, docsh_beam:source_file(B))
            end
    end.

get_beam(M, OriginalSourceFile) ->
    %% M is now rebuilt and/or reloaded from cache.
    {ok, Beam} = get_beam(M),
    {ok, docsh_beam:source_file(Beam, OriginalSourceFile)}.

-spec cached_or_rebuilt(docsh_beam:t(), file:name()) -> {ok, docsh_beam:t()}.
cached_or_rebuilt(Beam, CacheDir) ->
    %% TODO: find the module in cache, don't rebuild every time
    {ok, _RebuiltBeam} = rebuild(Beam, CacheDir).

ensure_cache_dir() ->
    CacheDir = cache_dir(),
    IsFile = filelib:is_file(CacheDir),
    IsDir = filelib:is_dir(CacheDir),
    case {IsFile, IsDir} of
        {true, true} ->
            CacheDir;
        {true, false} ->
            error(cache_location_is_not_a_dir);
        _ ->
            ok = file:make_dir(CacheDir),
            CacheDir
    end.

cache_dir() ->
    case {os:getenv("XDG_CACHE_HOME"), os:getenv("HOME")} of
        {false, false} -> error(no_cache_dir);
        {false, Home} -> filename:join([Home, ".docsh"]);
        {XDGCache, _} -> filename:join([XDGCache, "docsh"])
    end.

-spec reload(docsh_beam:t()) -> ok.
reload(Beam) ->
    BEAMFile = docsh_beam:beam_file(Beam),
    Path = filename:join([filename:dirname(BEAMFile),
                          filename:basename(BEAMFile, ".beam")]),
    unstick_module(docsh_beam:name(Beam)),
    {module, _} = code:load_abs(Path),
    stick_module(docsh_beam:name(Beam)),
    ok.

-spec rebuild(docsh_beam:t(), string()) -> any().
rebuild(B, CacheDir) ->
    BEAMFile = docsh_beam:beam_file(B),
    {ok, NewBEAM, Warnings} = docsh_lib:process_beam(BEAMFile),
    [ print("~s", [docsh_lib:format_error({W, docsh_beam:name(B)})]) || W <- Warnings ],
    NewBEAMFile = filename:join([CacheDir, filename:basename(BEAMFile)]),
    ok = file:write_file(NewBEAMFile, NewBEAM),
    docsh_beam:from_beam_file(NewBEAMFile).

unstick_module(Module) -> unstick_module(Module, code:is_sticky(Module)).

unstick_module(Module, true) -> code:unstick_mod(Module);
unstick_module(_,_) -> false.

stick_module(Module) -> stick_module(Module, code:is_sticky(Module)).

stick_module(Module, false) -> code:stick_mod(Module);
stick_module(_,_) -> false.
