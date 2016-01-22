-module(docsh_edoc).

-export([to_internal/1]).

to_internal(File) ->
    xmerl:export_simple([edoc(File)], docsh_edoc_xmerl).

edoc(File) ->
    {_Mod, EDoc} = edoc:get_doc(File, []),
    EDoc.
