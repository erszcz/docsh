P = "/home/erszcz/apps/elixir/1.2.1/bin/../lib/elixir/ebin/Elixir.Agent.beam".
{ok, {_, C}} = beam_lib:chunks(P, ["ExDc"]).
[{_, B}] = C.
Docs = binary_to_term(B).
{_ModLineNo, ModDoc} = proplists:get_value(moduledoc, element(2, DocsV1)).
FunDocs = proplists:get_value(docs, element(2, DocsV1)).
{{update,3}, _FunLineNo, _Type, _Something, FunDoc} = lists:keyfind({update,3}, 1, FunDocs).
