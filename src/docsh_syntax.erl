-module(docsh_syntax).

-behaviour(docsh_from).
-export([specs/1,
         types/1]).

-import(docsh_lib, [debug/3]).

-define(l(Args), fun () -> Args end).

-spec to_internal(file:filename()) -> docsh:internal().
to_internal(File) ->
    ok.

specs(Forms) ->
    debug(specs, [ iolist_to_binary(repr(T))
                   || T <- lists:flatmap(fun spec/1, Forms)]).

types(Forms) ->
    debug(types, [ iolist_to_binary(repr(T))
                   || T <- lists:flatmap(fun type/1, Forms)]).

spec({attribute,_,spec,_} = A) -> [A];
spec(_) -> [].

type({attribute,_,type,_} = A) -> [A];
type(_) -> [].

repr(TypeAttr) ->
    erl_pp:attribute(TypeAttr).

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.
