-export([h/1, h/2, h/3,
         s/1, s/2, s/3]).

h(ModOrFun) -> docsh_shell:h(ModOrFun).
h(M, F)     -> docsh_shell:h(M, F, any).
h(M, F, A)  -> docsh_shell:h(M, F, A).

s(Fun)      -> docsh_shell:s(Fun).
s(M, F)     -> docsh_shell:s(M, F, any).
s(M, F, A)  -> docsh_shell:s(M, F, A).
