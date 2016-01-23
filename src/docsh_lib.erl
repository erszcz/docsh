-module(docsh_lib).

-export([get/2, get/3,
         print/2]).

-type k() :: any().
-type v() :: any().

%% Error if Key is not found!
-spec get(k(), [{k(), v()}]) -> v().
get(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        false -> error(not_found, [Key, Props]);
        {Key, Val} -> Val
    end.

-spec get(k(), [{k(), v()}], v()) -> v().
get(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        false -> Default;
        {Key, Val} -> Val
    end.

-spec print(io:format(), [term()]) -> ok.
print(Fmt, Args) ->
    io:format(Fmt, Args).
