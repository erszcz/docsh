-module(docsh_shell).

-export([h/1, h/3,
         s/1, s/3,
         t/3]).

-import(docsh_embeddable, [key_to_module/1]).
-import(docsh_lib, [print/2]).

-type lookup_params() :: [lookup_param()].
-type lookup_param() :: moduledoc | doc | spec | type.

-spec h(fun() | module()) -> ok.
h(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    h(M, F, A);

h(M) when is_atom(M) ->
    lookup(M, [moduledoc]).

h(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [doc, spec]).

s(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    s(M, F, A).

s(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [spec]).

t(M) when is_atom(M) ->
    lookup(M, [type]).

t(M, T, Arity) when is_atom(M), is_atom(T),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, T, Arity}, [type]).

lookup(Key, Args) ->
    case get_beam(key_to_module(Key)) of
        {error, R} -> error(R, Key);
        {ok, Beam} ->
            check_edoc_availability(Beam, Args),
            docsh_embeddable:lookup(Key, Args)
    end.

-spec check_edoc_availability(docsh_beam:t(), lookup_params()) -> ok.
check_edoc_availability(Beam, LParams) ->
    case {proplists:get_value(doc, LParams, false), docsh_beam:source_file(Beam)} of
        {true, false} ->
            print("\nSource file for ~s is not available.\n",
                  [docsh_beam:name(Beam)]);
        _ -> ok
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
    {ok, NewBEAM} = docsh_lib:process_beam(BEAMFile),
    NewBEAMFile = filename:join([CacheDir, filename:basename(BEAMFile)]),
    ok = file:write_file(NewBEAMFile, NewBEAM),
    docsh_beam:from_beam_file(NewBEAMFile).

unstick_module(Module) -> unstick_module(Module, code:is_sticky(Module)).

unstick_module(Module, true) -> code:unstick_mod(Module);
unstick_module(_,_) -> false.

stick_module(Module) -> stick_module(Module, code:is_sticky(Module)).

stick_module(Module, false) -> code:stick_mod(Module);
stick_module(_,_) -> false.
