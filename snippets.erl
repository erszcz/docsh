{ok, {_, C}} = beam_lib:chunks("/Users/erszcz/apps/elixir/xyz/bin/../lib/elixir/ebin/Elixir.Agent.beam", ["ExDc"]).
[{_, B}] = C.
DocsV1 = binary_to_term(B).
{_ModLineNo, ModDoc} = proplists:get_value(moduledoc, element(2, DocsV1)).
FunDocs = proplists:get_value(docs, element(2, DocsV1)).
{{update,3}, _FunLineNo, _Type, _Something, FunDoc} = lists:keyfind({update,3}, 1, FunDocs).
