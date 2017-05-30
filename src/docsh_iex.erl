-module(docsh_iex).

-export([get_docs/2]).

-type kind() :: docs
              | moduledoc
              | callback_docs
              | type_docs
              | all.

-type error() :: {error, any()}.

-spec get_docs(module(), kind()) -> {any(), any()} | error().
get_docs(Mod, moduledoc) ->
    {ok, docsh_shell:unchecked_lookup2([Mod], [Mod])};
get_docs(_Mod, _) ->
    {error, not_implemented}.
