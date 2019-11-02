-module(docsh_edoc_xmerl_flat).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-import(docsh_lib, [print/2]).

-include_lib("docsh/include/docsh_exdoc.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> any().
'#root#'(Data, _Attrs, [], _E) -> lists:reverse(Data).

%% The '#element#' function is the default handler for XML elements.
-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(Tag, Data, _Attrs, Parents, _E) ->
    docsh_lib:debug(flat, "~p ~p ~p~n", [Tag, Data, Parents]),
    [{Tag, Parents}].

%% Unused.
-spec '#xml-inheritance#'() -> any().
'#xml-inheritance#'() -> [].
