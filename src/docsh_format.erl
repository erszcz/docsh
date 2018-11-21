-module(docsh_format).

-export([lookup/3,
         module_doc_not_available/0,
         module_doc_hidden/0,
         item_doc_not_available/0,
         item_doc_hidden/0]).

-export_type([error_message/0]).

-type t() :: any().
-type key() :: module() | kna() | docsh_internal:key().
-type kna() :: {kind(), atom(), arity()}.
-type kind() :: function | type | callback  %% EEP-48 general
              | macro.                      %% Elixir/LFE
-type kinds() :: [docsh_internal:item_kind()].

-type error_message() :: binary().

-callback lookup(docsh_format:t(), key(), kinds()) -> {ok, binary()}
                                                    | {not_found, error_message()}.

-spec lookup(t(), key(), kinds()) -> {ok, binary()} | {not_found, error_message()}.
lookup(Docs, Key, Items) ->
    KnownFormats = application:get_env(docsh, extra_docs_chunk_formats, []) ++ default_formats(),
    DocsFormat = element(1, Docs),
    case lists:keyfind(DocsFormat, 1, KnownFormats) of
        {DocsFormat, FormatMod} ->
            FormatMod:lookup(Docs, Key, Items);
        false ->
            erlang:error({unknown_docs_format, DocsFormat}, [Docs, Key, Items])
    end.

default_formats() ->
    [
     {docs_v1, docsh_docs_v1}
    ].

-spec module_doc_not_available() -> binary().
module_doc_not_available() ->
    <<"Module description is not available.\n">>.

-spec module_doc_hidden() -> binary().
module_doc_hidden() ->
    <<"Module description is hidden.\n">>.

-spec item_doc_not_available() -> binary().
item_doc_not_available() ->
    <<"Item description is not available.\n">>.

-spec item_doc_hidden() -> binary().
item_doc_hidden() ->
    <<"Item description is hidden.\n">>.
