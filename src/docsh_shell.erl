-module(docsh_shell).

-export([h/1, h/3]).

-spec h(fun() | module()) -> ok.
h(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    h(M, F, A);

h(M) when is_atom(M) ->
    BEAMFile = code:which(M),
    case docsh_lib:has_exdc(BEAMFile) of
        true -> M:h();
        false ->
            reload_with_exdc(M, BEAMFile),
            docsh_embeddable:h(M)
    end.

h(M, F, A) ->
    BEAMFile = code:which(M),
    case docsh_lib:has_exdc(BEAMFile) of
        true -> M:h(F, A);
        false ->
            reload_with_exdc(M, BEAMFile),
            docsh_embeddable:h(M)
    end.

reload_with_exdc(M, BEAMFile) ->
    {ok, NewBEAM} = docsh_lib:process_beam(BEAMFile),
    unstick_module(M),
    {module, M} = code:load_binary(M, BEAMFile, NewBEAM),
    stick_module(M).

%% Stolen from meck:
%% https://github.com/eproxus/meck/blob/8902e5c9cdc5b3fca38cee830daaa15d1566167f/src/meck_proc.erl#L557
unstick_module(Module) -> unstick_module(Module, code:is_sticky(Module)).

unstick_module(Module, true) -> code:unstick_mod(Module);
unstick_module(_,_) -> false.

stick_module(Module) -> stick_module(Module, code:is_sticky(Module)).

stick_module(Module, false) -> code:stick_mod(Module);
stick_module(_,_) -> false.
