-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

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
    [{function, (function_details_from_attrs(Attrs) ++
                 function_details_from_data(Data))}];
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
        when Tag =:= p;
             Tag =:= fullDescription ->
     strip_leading_whitespace(?il2b(Data));
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= a;
             Tag =:= code;
             Tag =:= dd;
             Tag =:= dl;
             Tag =:= dt;
             Tag =:= li;
             Tag =:= ul ->
    %% Just get the text.
    Data;
'#element#'(Tag, Data, _Attrs, _Parents, _E) ->
    [{Tag, Data}].

strip_leading_whitespace(BString) ->
    String = binary_to_list(BString),
    Lines = string:tokens(String, "\n"),
    ?il2b(string:join([ string:strip(L) || L <- Lines ], "\n")).

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

function_details_from_attrs(Attrs) ->
    [ function_detail(At) || At <- Attrs ].

function_detail(#xmlAttribute{name = name} = At) ->
    {name, ?l2ea(value(At))};
function_detail(#xmlAttribute{name = arity} = At) ->
    {arity, ?l2i(value(At))};
function_detail(#xmlAttribute{name = exported} = At) ->
    {exported, string_to_boolean(value(At))};
function_detail(#xmlAttribute{name = label} = At) ->
    {label, ?l2b(value(At))}.

function_details_from_data(Data) ->
    lists:flatmap(fun function_detail_from_data/1, Data).

function_detail_from_data([{description, _}] = Desc) ->
    Desc;
function_detail_from_data(_) -> [].

value(#xmlAttribute{value = V}) -> V.

string_to_boolean("yes") -> true;
string_to_boolean("no") -> false.
