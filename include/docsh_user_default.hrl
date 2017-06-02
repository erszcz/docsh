-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/2, t/3]).

h(ModOrFun) -> docsh_erl:h(ModOrFun).
h(M, F)     -> docsh_erl:h(M, F, any).
h(M, F, A)  -> docsh_erl:h(M, F, A).

s(Fun)      -> docsh_erl:s(Fun).
s(M, F)     -> docsh_erl:s(M, F, any).
s(M, F, A)  -> docsh_erl:s(M, F, A).

t(M, T)     -> docsh_erl:t(M, T, any).
t(M, T, A)  -> docsh_erl:t(M, T, A).
