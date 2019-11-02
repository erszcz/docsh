-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/1, t/2, t/3]).

-spec h(fun() | module()) -> ok.
h(ModOrFun) -> docsh_erl:h(ModOrFun).

-spec h(module(), docsh_erl:name()) -> ok.
h(M, F)     -> docsh_erl:h(M, F).

-spec h(module(), docsh_erl:name(), arity() | 'any') -> ok.
h(M, F, A)  -> docsh_erl:h(M, F, A).

-spec s(fun()) -> ok.
s(Fun)      -> docsh_erl:s(Fun).

-spec s(module(), docsh_erl:name()) -> ok.
s(M, F)     -> docsh_erl:s(M, F).

-spec s(module(), docsh_erl:name(), arity() | 'any') -> ok.
s(M, F, A)  -> docsh_erl:s(M, F, A).

-spec t(module()) -> ok.
t(M)        -> docsh_erl:t(M).

-spec t(module(), docsh_erl:name()) -> ok.
t(M, T)     -> docsh_erl:t(M, T).

-spec t(module(), docsh_erl:name(), arity() | 'any') -> ok.
t(M, T, A)  -> docsh_erl:t(M, T, A).
