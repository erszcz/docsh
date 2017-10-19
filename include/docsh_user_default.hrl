-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/1, t/2, t/3]).

h(ModOrFun) -> docsh_erl:h(ModOrFun).
h(M, F)     -> docsh_erl:h(M, F).
h(M, F, A)  -> docsh_erl:h(M, F, A).

s(Fun)      -> docsh_erl:s(Fun).
s(M, F)     -> docsh_erl:s(M, F).
s(M, F, A)  -> docsh_erl:s(M, F, A).

t(M)        -> docsh_erl:t(M).
t(M, T)     -> docsh_erl:t(M, T).
t(M, T, A)  -> docsh_erl:t(M, T, A).
