-module(docsh_edoc).
-behaviour(docsh_from).

-export([to_internal/1]).

-spec to_internal(file:filename()) -> docsh:internal().
to_internal(File) ->
    xmerl:export_simple([edoc(File)], docsh_edoc_xmerl).

edoc(File) ->
    {_Mod, EDoc} = edoc:get_doc(File, []),
    EDoc.
