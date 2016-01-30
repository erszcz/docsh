-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-export([fullDescription/4]).

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
'#element#'(equiv, Data, _Attrs, _Parents, _E) ->
    {see, [Reference]} = lists:keyfind(see, 1, lists:flatten(Data)),
    [{description, ?il2b([<<"See ">>, Reference])}];
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
    %% Flatten all text content from child elements.
    [{description, ?il2b(Data)}];
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= a;
             Tag =:= code;
             Tag =:= dd;
             Tag =:= dl;
             Tag =:= dt;
             Tag =:= em;
             Tag =:= h4;
             Tag =:= h5;
             Tag =:= h6;
             Tag =:= li;
             Tag =:= ol;
             Tag =:= tt;
             Tag =:= ul ->
    %% Discard.
    [];
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= p ->
     {fmt, debug(Tag, cleanup_lines(Data))};
'#element#'(Tag, Data, _Attrs, _Parents, E) when
        Tag =:= pre ->
     {fmt, debug(Tag, Data)};
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= h1;
             Tag =:= h2;
             Tag =:= h3 ->
    header(Tag, Data);
'#element#'(Tag, Data, _Attrs, _Parents, _E) ->
    [{Tag, Data}].

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

strip_whitespace(BString) ->
    String = binary_to_list(BString),
    Lines = string:tokens(String, "\n"),
    ?il2b(string:join([ string:strip(L) || L <- Lines ], "\n")).

cleanup_lines(BString) ->
    Lines = re:replace(BString, <<"\s*\n\s*">>, <<"\n">>, [global, {return, list}]),
    ?il2b(string:strip(lists:flatten(Lines), both, $\n)).

fullDescription(Data, _Attrs, _Parents, _E) ->
    [{fmt, H} | T] = collect_loose_text(Data),
    ?il2b([H] ++ [ ["\n\n", E] || {fmt, E} <- T ]).

collect_loose_text(Data) ->
    debug(loose, collect_loose_text(Data, [], [])).

collect_loose_text([], [], Fmt) -> Fmt;
collect_loose_text([], Data, Fmt) ->
    [{fmt, cleanup_lines(Data)} | Fmt];
collect_loose_text([{fmt, Element} | T], [], Fmt) ->
    [{fmt, Element} | collect_loose_text(T, [], Fmt)];
collect_loose_text([{fmt, Element} | T], Data, Fmt) ->
    Clean = cleanup_lines(?il2b(lists:reverse(Data))),
    case Clean of
        <<>> -> [{fmt, Element}];
        _ -> [{fmt, Element}, {fmt, Clean}]
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
