-module(docsh_lib).

-export([print/2]).

print(Fmt, Args) ->
    io:format(Fmt, Args).
