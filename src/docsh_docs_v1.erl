%% @doc EEP-0048 (http://erlang.org/eeps/eep-0048.html) docs_v1 documentation format support.
-module(docsh_docs_v1).

-behaviour(docsh_format).
-export([lookup/3]).

-export_type([t/0]).

-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs}).

-opaque t() :: #docs_v1{anno :: erl_anno:anno(),
                        beam_language :: atom(),
                        format :: mime_type(),
                        module_doc :: i18n_doc() | none | hidden,
                        metadata :: map(),
                        docs :: [item()]}.

-type mime_type() :: binary().

-type item() :: {KNA :: docsh_format:kna(),
                 Anno :: erl_anno:anno(),
                 Signature :: [binary()],
                 Doc :: i18n_doc() | none | hidden,
                 Metadata :: map()}.

-ifndef(no_map_specs).
-type i18n_doc() :: #{language() := string_()}.
-type language() :: binary().
-type string_() :: binary().
-else.
-type i18n_doc() :: #{}.
-endif.

-spec lookup(docsh_format:t(), docsh_format:key(), docsh_format:kinds()) -> [binary()].
lookup(#docs_v1{} = Docs, _Key, _Kinds) ->
    error(not_implemented),
    {not_found, <<"docsh: docs_v1 lookup not implemented">>}.
