%% @doc EEP-0048 (http://erlang.org/eeps/eep-0048.html) docs_v1 documentation format support.
-module(docsh_docs_v1).

-behaviour(docsh_format).
-export([lookup/2,
         merge/1]).

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
                        module_doc :: #{language() := string_()} | none | hidden,
                        metadata :: map(),
                        docs :: [item()]}.

-type mime_type() :: binary().

-type item() :: {KNA :: docsh_format:kna(),
                 Anno :: erl_anno:anno(),
                 Signature :: [binary()],
                 Doc :: #{language() := string_()} | none | hidden,
                 Metadata :: map()}.

-type language() :: binary().
-type string_() :: binary().

-spec lookup(docsh_beam:t(), docsh_format:kna()) -> [binary()].
lookup(_Beam, _Key) ->
    error(not_implemented),
    [].

-spec merge([t()]) -> t().
merge(_) ->
    error(not_implemented),
    #docs_v1{}.
