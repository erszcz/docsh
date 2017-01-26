-module(docsh_shell).

-export([h/1, h/3,
         s/3]).

-spec h(fun() | module()) -> ok.
h(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    h(M, F, A);

h(M) when is_atom(M) ->
    case get_beam(M) of
        {error, R} -> error(R, [M]);
        {ok, B} -> erlang:apply(docsh_embeddable, h, [M])
    end.

h(M, F, Arity) when is_atom(M), is_atom(F), is_integer(Arity) ->
    case get_beam(M) of
        {error, R} -> error(R, [M, F, Arity]);
        {ok, B} -> erlang:apply(docsh_embeddable, h, [M, F, Arity, [doc, spec]])
    end.

s(M, F, Arity) when is_atom(M), is_atom(F), is_integer(Arity) ->
    case get_beam(M) of
        {error, R} -> error(R, [M, F, Arity]);
        {ok, B} -> erlang:apply(docsh_embeddable, h, [M, F, Arity, [spec]])
    end.

get_beam(M) -> get_beam(M, init).

get_beam(M, Attempt) when Attempt =:= init;
                          Attempt =:= retry ->
    case docsh_beam:from_loadable_module(M) of
        {error, _} = E -> E;
        {ok, B} ->
            case {Attempt, docsh_lib:has_exdc(docsh_beam:beam_file(B))} of
                {_, true} -> {ok, B};
                {init, false} ->
                    {ok, NewB} = cached_or_rebuilt(B, cache_dir()),
                    reload(NewB),
                    get_beam(M, retry)
            end
    end.

-spec cached_or_rebuilt(docsh_beam:t(), file:name()) -> {ok, docsh_beam:t()}.
cached_or_rebuilt(Beam, CacheDir) ->
    %% TODO: find the module in cache, don't rebuild every time
    {ok, _RebuiltBeam} = rebuild(Beam, CacheDir).

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
