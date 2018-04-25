-module(docsh_format).

-export([lookup/3]).

-export_type([kna/0]).

-type t() :: any().
-type kna() :: {atom(), atom(), arity()}.

-callback lookup(docsh_beam:t(), kna()) -> [binary()].
-callback merge([t()]) -> t().

%-spec lookup(docsh_beam:t(), Key, Items) -> [binary()].
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
