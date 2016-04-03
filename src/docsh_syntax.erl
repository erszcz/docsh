-module(docsh_syntax).

-behaviour(docsh_reader).
-export([to_internal/1]).
-export([specs/1,
         types/1]).

%% Internal.
-export([spec/1,
         type/1]).

-import(docsh_lib, [debug/3]).

-type ast() :: [form()].
-type form() :: erl_parse:abstract_form().

%% TODO: temporary type
-type z() :: {{spec | type, {atom(), arity()}}, {description, binary()}}.

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l(Args), fun () -> Args end).

-spec to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh:internal()}
         | {error, any()}.
to_internal(Beam) ->
    try
        Forms = case {docsh_beam:abst(Beam), docsh_beam:source_file(Beam)} of
                    {false, false} ->
                        error(no_debug_info_no_src, [Beam]);
                    {Abst, false} ->
                        Abst;
                    {_, Source} ->
                        {ok, Fs} = epp:parse_file(Source, []),
                        Fs
                end,
        {ok, [{module, [{name, module_name(Forms)}]}] ++ specs(Forms) ++ types(Forms)}
    catch
        _:R -> {error, R}
    end.

module_name(Forms) ->
    case lists:keyfind(module, 3, Forms) of
        false -> error(not_found, [Forms]);
        {_, _, module, Mod} -> Mod
    end.

-spec specs(ast()) -> [z()].
specs(Forms) -> attrs(spec, Forms).

-spec types(ast()) -> [z()].
types(Forms) -> attrs(type, Forms).

-spec attrs(spec | type, ast()) -> [z()].
attrs(AttrName, Forms) ->
    debug(AttrName, [ {{AttrName, name_arity(AttrName, F)},
                       {description, ?il2b(desc(F))}}
                      || F <- lists:flatmap(fun ?MODULE:AttrName/1, Forms) ]).

desc(Attr) ->
    debug('repr:type', format(Attr)).

-spec spec(form()) -> [form()].
spec(Attr) -> attr(spec, Attr).

-spec type(form()) -> [form()].
type(Attr) -> attr(type, Attr).

attr(AttrName, {attribute,_,AttrName,_} = A) -> [A];
attr(_, _) -> [].

-spec name_arity(spec | type, form()) -> {atom(), arity()}.
name_arity(type, {attribute, _, type, Data}) ->
    debug('name_arity:in', Data),
    {Name, _, Args} = Data,
    %% TODO: how to extract type arity? is this correct?
    {Name, length(Args)};
name_arity(spec, {attribute, _, spec, Data}) ->
    {NameArity, _} = Data,
    NameArity.

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

format(Attr) ->
    erl_pp:form(Attr).
