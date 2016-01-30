-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-export([fullDescription/4,
         li/4,
         ol/4,
         ul/4]).

-record(function, {name, arity, exported, label, description}).

-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%% The '#text#' function is called for every text segment.
'#text#'(Text) -> ?il2b(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
'#root#'(Data, _Attrs, [], _E) ->
    lists:flatten(Data).

%% The '#element#' function is the default handler for XML elements.
'#element#'(function, Data, Attrs, _Parents, _E) ->
    F1 = function_details_from_attrs(Attrs, #function{}),
    F2 = function_details_from_data(Data, F1),
    [{function, {{F2#function.name, F2#function.arity},
                 {exported,    F2#function.exported},
                 {label,       F2#function.label},
                 {description, F2#function.description}}}];
'#element#'(equiv, [Reference], _Attrs, _Parents, _E) ->
    debug(equiv, Reference),
    [{description, [<<"See ">>, Reference]}];
'#element#'(functions, Data, _Attrs, _Parents, _E) ->
    %% Functions are already extracted.
    [{functions, Data}];
'#element#'(module, Data, Attrs, _Parents, _E) ->
    Details = module_details_from_data(Data),
    {value, {functions, Functions}, Rest} = lists:keytake(functions, 1, Details),
    [{module, module_details_from_attrs(Attrs) ++ Rest}
     | Functions];
'#element#'(briefDescription, _Data, _Attrs, _Parents, _E) ->
    [];
'#element#'(description, Data, _Attrs, _Parents, _E) ->
    [{description, debug(description, ?il2b(Data))}];
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= h1;
             Tag =:= h2;
             Tag =:= h3 ->
    {fmt, debug(inline, header(Tag, unwrap_inline(Data)))};
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= a;
             Tag =:= code;
             Tag =:= em;
             Tag =:= expr;
             Tag =:= h4;
             Tag =:= h5;
             Tag =:= h6;
             Tag =:= tt ->
    debug('inline:before', Data),
    {inline, debug('inline:after', [ unwrap_inline(E) || E <- Data ])};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= p ->
    {fmt, debug(Tag, cleanup_lines(Data))};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= pre ->
    {fmt, debug(Tag, [Data, "\n"])};
'#element#'(Tag, Data, _Attrs, _Parents, _E) ->
    debug(discarded, {Tag, Data}),
    [].

unwrap_inline([]) -> [];
unwrap_inline([{inline, Elements}]) when is_list(Elements) -> Elements;
unwrap_inline({inline, Elements}) when is_list(Elements) -> Elements;
unwrap_inline([BString]) when is_binary(BString) -> BString;
unwrap_inline(BString) when is_binary(BString) -> BString.

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

cleanup_lines(BString) when is_binary(BString) ->
    Lines = re:replace(BString, <<"\s*\n\s*">>, <<"\n">>, [global, {return, list}]),
    S = string:strip(lists:flatten(Lines), both, $\n),
    [ [T, "\n"] || T <- string:tokens(S, "\n") ];
cleanup_lines(IOList) ->
    BString = ?il2b([ unwrap_inline(E) || E <- IOList ]),
    cleanup_lines(BString).

fullDescription(Data, _Attrs, _Parents, _E) ->
    [{fmt, H} | T] = collect_loose_text(Data),
    ?il2b([H] ++ [ ["\n", E] || {fmt, E} <- T ]).

li({fmt, Formatted}, _Attrs, _Parents, _E) ->
    {fmt, debug(li, Formatted)};
li(Data, _Attrs, _Parents, _E) ->
    {fmt, debug(li, case collect_loose_text(Data) of
                        [] -> [];
                        Formatted ->
                            lists:append([ unwrap_fmt(E) || E <- Formatted ])
                    end)}.

ol(Data, _Attrs, _Parents, _E) -> list(ol, Data).

ul(Data, _Attrs, _Parents, _E) -> list(ul, Data).

list(Type, Data) ->
    Items = enumerate([ Unwrapped
                        || E <- Data,
                           [_|_] = Unwrapped <- [unwrap_fmt(E)] ]),
    {fmt, debug(Type, [ [bullet(Type, I, L), Item]
                        || {I, E} <- Items,
                           {L, Item} <- enumerate(E) ])}.

bullet(ul, _Item, 1) -> "  - ";
bullet(ul, _Item, _) -> "    ";
bullet(ol,  Item, 1) -> io_lib:format("  ~b. ", [Item]);
bullet(ol, _Item, _) -> "    ".

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

unwrap_fmt({fmt, Lines}) -> Lines;
unwrap_fmt({fmt, []}) -> [];
unwrap_fmt(_) -> [].

collect_loose_text(Data) ->
    debug(loose, collect_loose_text(Data, [], [])).

collect_loose_text([], [], Fmt) -> Fmt;
collect_loose_text([], Data, Fmt) ->
    [{fmt, cleanup_lines(?il2b(lists:reverse(Data)))} | Fmt];
collect_loose_text([{inline, Element} | T], [], Fmt) ->
    collect_loose_text(T, [Element], Fmt);
collect_loose_text([{fmt, Element} | T], [], Fmt) ->
    [{fmt, Element} | collect_loose_text(T, [], Fmt)];
collect_loose_text([{inline, Element} | T], Data, Fmt) ->
    collect_loose_text(T, [Element | Data], Fmt);
collect_loose_text([{fmt, Element} | T], Data, Fmt) ->
    Clean = cleanup_lines(?il2b(lists:reverse(Data))),
    case Clean of
        [] -> [{fmt, Element}];
        _ -> [{fmt, Clean}, {fmt, Element}]
    end ++ collect_loose_text(T, [], Fmt);
collect_loose_text([LooseText | T], Data, Fmt) when is_binary(LooseText);
                                                    is_list(LooseText) ->
    collect_loose_text(T, [LooseText | Data], Fmt).

header(Level, Data) ->
    [header_prefix(Level), Data].

header_prefix(h1) -> <<"# ">>;
header_prefix(h2) -> <<"## ">>;
header_prefix(h3) -> <<"### ">>.

%% Unused.
'#xml-inheritance#'() -> [].

module_details_from_attrs(Attrs) ->
    [ D || At <- Attrs, D <- [module_detail_from_attr(At)], D /= ignore ].

module_detail_from_attr(#xmlAttribute{name = name} = At) ->
    {name, ?l2ea(value(At))};
module_detail_from_attr(#xmlAttribute{}) ->
    ignore.

module_details_from_data(Data) ->
    lists:flatmap(fun module_detail_from_data/1, Data).

module_detail_from_data([{description, _}] = Desc) ->
    Desc;
module_detail_from_data([{functions, _}] = Functions) ->
    Functions;
module_detail_from_data(_) -> [].

function_details_from_attrs(Attrs, F) ->
    lists:foldl(fun fda/2, F, Attrs).

fda(#xmlAttribute{name = name} = At, F)     -> F#function{name = ?l2ea(value(At))};
fda(#xmlAttribute{name = arity} = At, F)    -> F#function{arity = ?l2i(value(At))};
fda(#xmlAttribute{name = exported} = At, F) -> F#function{exported = string_to_boolean(value(At))};
fda(#xmlAttribute{name = label} = At, F)    -> F#function{label = ?l2b(value(At))}.

function_details_from_data(Data, F) ->
    lists:foldl(fun fdd/2, F, Data).

fdd([{description, Desc}], F) -> F#function{description = Desc};
fdd(_, F)                     -> F.

value(#xmlAttribute{value = V}) -> V.

string_to_boolean("yes") -> true;
string_to_boolean("no") -> false.
