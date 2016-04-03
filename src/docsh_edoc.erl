-module(docsh_edoc).

-behaviour(docsh_reader).
-export([to_internal/1]).

-export([flat/1]).

-import(docsh_lib, [debug/3]).

-define(l(Args), fun () -> Args end).

-spec to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh:internal()}
         | {error, any()}.
to_internal(Beam) ->
    try
        File = case docsh_beam:source_file(Beam) of
                   false -> error(edoc_requires_source);
                   F when is_list(F) -> F
               end,
        EDoc = edoc(File),
        debug(edoc, "edoc:~n~p~n~n", ?l([EDoc])),
        debug(xml,  "xml:~n~s~n~n",  ?l([xmerl:export_simple([EDoc], xmerl_xml)])),
        debug(html, "html:~n~s~n~n",  ?l([edoc:layout(EDoc)])),
        Internal = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
        debug(internal, "internal:~n~p~n~n", [Internal]),
        {ok, Internal}
    catch
        _:R -> {error, R}
    end.

edoc(File) ->
    {_Mod, EDoc} = edoc:get_doc(File, []),
    EDoc.

-spec flat(file:filename()) -> any().
flat(File) ->
    EDoc = edoc(File),
    xmerl:export_simple([EDoc], docsh_edoc_xmerl_flat).
