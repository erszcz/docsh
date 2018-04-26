-module(docsh_format).

-export([lookup/3]).

-type t() :: any().
-type key() :: module() | kna() | docsh_internal:key().
-type kna() :: {kind(), atom(), arity()}.
-type kind() :: function | type | callback  %% EEP-48 general
              | macro.                      %% Elixir/LFE
-type kinds() :: [docsh_internal:item_kind()].

-type error_message() :: binary().

-callback lookup(docsh_format:t(), key(), kinds()) -> [binary()].

-spec lookup(docsh_beam:t(), key(), kinds()) -> {ok, binary()} | {not_found, error_message()}.
lookup(Beam, Key, Items) ->
    KnownFormats = application:get_env(docsh, docs_chunk_formats, default_formats()),
    Docs = docsh_beam:docs(Beam),
    DocsFormat = element(1, Docs),
    case lists:keyfind(DocsFormat, 1, KnownFormats) of
        {DocsFormat, FormatMod} ->
            FormatMod:lookup(Docs, Key, Items);
        false ->
            error({unknown_docs_format, DocsFormat}, [Beam, Key, Items])
    end.

default_formats() ->
    [
     {docsh_docs_v1, docsh_internal}
     %% TODO: add docsh_docs_v1
     %{docs_v1, docsh_docs_v1}
    ].
