-module(docsh_edoc_xmerl).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 '#xml-inheritance#'/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%% The '#text#' function is called for every text segment.
'#text#'(Text) -> iolist_to_binary(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
'#root#'([{module, Data, Attrs}], _Attrs, [], _E) ->
    {functions, Functions, _} = lists:keyfind(functions, 1, Data),
    [{module, (module_details_from_attrs(Attrs) ++
	       module_details_from_data(Data))}] ++ Functions.

%% The '#element#' function is the default handler for XML elements.
'#element#'(function, Data, Attrs, _Parents, _E) ->
    {function, (function_details_from_attrs(Attrs) ++
	        function_details_from_data(Data))};
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    {Tag, Data, Attrs}.

%% Unused.
'#xml-inheritance#'() -> [].

module_details_from_attrs(Attrs) ->
    [ D || At <- Attrs, D <- [module_detail_from_attr(At)], D /= ignore ].

module_detail_from_attr(#xmlAttribute{name = name} = At) ->
    {name, ?l2ea(value(At))};
module_detail_from_attr(#xmlAttribute{}) ->
    ignore.

module_details_from_data(Data) ->
    [ D || Datum <- Data, D <- [module_detail_from_data(Datum)], D /= ignore ].

module_detail_from_data({description, _, _} = Desc) ->
    description(Desc);
module_detail_from_data(_) ->
    ignore.

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
    [ D || Datum <- Data, D <- [function_detail_from_data(Datum)], D /= ignore ].

function_detail_from_data({description, _, _} = Desc) ->
    description(Desc);
function_detail_from_data(_) ->
    ignore.

description({description, Desc, _}) ->
    [{briefDescription, _, _}, {fullDescription, [Full], _}] = Desc,
    {description, Full}.

value(#xmlAttribute{value = V}) -> V.

string_to_boolean("yes") -> true;
string_to_boolean("no") -> false.
