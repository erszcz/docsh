-module(docsh_lib).

-export([convert/3,
         get/2, get/3,
         debug/3,
         print/2, print/3,
         join/2]).

-type k() :: any().
-type v() :: any().

-spec convert(FromMods, ToMod, AST) -> docsh:external() when
      FromMods :: [module()],
      ToMod :: module(),
      AST :: [erl_parse:abstract_form()].
convert(FromMods, ToMod, AST) ->
    Internal = docsh_internal:merge([ FromMod:to_internal(file(AST))
                                      || FromMod <- FromMods ]),
    ToMod:from_internal(Internal).

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
    print(standard_io, Fmt, Args).

-spec print(io:device(), io:format(), [term()]) -> ok.
print(Handle, Fmt, Args) ->
    io:format(Handle, Fmt, Args).

-spec debug(atom(), io:format(), [term()] | fun(() -> [term()])) -> ok.
debug(Tag, Fmt, Args) ->
    case os:getenv("DOCSH_DEBUG") of
        false -> ok;
        Tags -> debug_matching(string:tokens(Tags, ","),
                               atom_to_list(Tag), Fmt, Args)
    end.

debug_matching(Tags, Tag, Fmt, Args) ->
    case lists:member(Tag, Tags) of
        false -> ok;
        true -> print(Fmt, if
                               is_function(Args, 0) -> Args();
                               is_list(Args) -> Args
                           end)
    end.

-spec join([v()], v()) -> [v()].
join([], _Sep) -> [];
join([H], _Sep) -> [H];
join([H|T], Sep) -> [H, Sep | join(T, Sep)].

file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.
