-module(docsh_syntax).

-behaviour(docsh_reader).
-export([to_internal/1]).
-export([specs/1,
         types/1]).

-import(docsh_lib, [debug/3]).

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l(Args), fun () -> Args end).

-spec to_internal(file:filename()) -> docsh:internal().
to_internal(File) ->
    {ok, Forms} = epp:parse_file(File, []),
    %specs(Forms),
    [{module, [{name, module_name(Forms)}]}] ++ types(Forms).

module_name(Forms) ->
    case catch find_module_name(Forms) of
        '$__not_found__' -> error(not_found, [Forms]);
        Mod when is_atom(Mod) -> Mod
    end.

find_module_name([]) -> throw('$__not_found__');
find_module_name([{attribute, _, module, Mod} | _Forms]) -> throw(Mod);
find_module_name([_ | Forms]) -> find_module_name(Forms).

specs(Forms) ->
    [].
    %debug(specs, [ iolist_to_binary(desc(T))
    %               || T <- lists:flatmap(fun spec/1, Forms)]).

types(Forms) ->
    debug(types, [ {{type, type_name_arity(T)}, {description, ?il2b(desc(T))}}
                   || T <- lists:flatmap(fun type/1, Forms)]).

spec({attribute,_,spec,_} = A) -> [A];
spec(_) -> [].

type({attribute,_,type,_} = A) -> [A];
type(_) -> [].

%desc({attribute,_,spec,_} = A) -> docsh_format:spec_attr(A);
desc({attribute,_,type,_} = A) -> debug('repr:type', docsh_format:type_attr(A)).

type_name_arity({attribute,_,type,Data}) ->
    {Name, _, Args} = Data,
    %% TODO: how to extract type arity? is this correct?
    {Name, length(Args)}.

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.
