-compile([{parse_transform, pt_docsh}]).

%% TODO: this could be added at compile time by pt_docsh,
%%       but somehow it doesn't work,
%%       i.e. breaks the compiler with {1,erl_lint,{undefined_function,[{h,0}]}},
%%       though h/0 is also generated at compile time and embedded in the module
-export([h/0, h/2]).
