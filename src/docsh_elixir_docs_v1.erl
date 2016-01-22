-module(docsh_elixir_docs_v1).
-export([to_external/1]).

to_external(Internal) ->
    {elixir_docs_v1, fake_docs}.
