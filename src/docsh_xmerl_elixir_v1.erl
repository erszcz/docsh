-module(docsh_xmerl_elixir_v1).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 '#xml-inheritance#'/0]).

%% The '#text#' function is called for every text segment.
'#text#'(Text) -> iolist_to_binary(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
'#root#'(Data, _Attrs, [], _E) -> Data.

%% The '#element#' function is the default handler for XML elements.
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    {Tag, Data, Attrs}.

%% Unused.
'#xml-inheritance#'() -> [].
