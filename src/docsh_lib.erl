-module(docsh_lib).

-export([get/2,
         print/2]).

get(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        false -> error(not_found, [Key, Props]);
        {Key, Val} -> Val
    end.

print(Fmt, Args) ->
    io:format(Fmt, Args).
