-module(docsh_edoc).

-export([to_internal/1]).

-import(docsh_lib, [print/2]).

to_internal(File) ->
    xmerl:export_simple([edoc(File)], docsh_edoc_xmerl).

edoc(File) ->
    {_Mod, EDoc} = edoc:get_doc(File, []),
    print("edoc xml: ~s~n", [xml(EDoc)]),
    EDoc.

xml(EDoc) ->
    iolist_to_binary(xmerl:export_simple([EDoc], xmerl_xml)).
