-module(docsh_edoc).

-behaviour(docsh_reader).
-export([available/1,
         to_internal/1]).

%% Test API
-export([to_internal/2]).

-import(docsh_lib, [print/2]).

-define(l(Args), fun () -> Args end).

-spec available(docsh_beam:t()) -> [docsh_reader:t()].
available(Beam) ->
    [ ?MODULE || docsh_lib:is_module_available(edoc),
                 docsh_lib:is_module_available(xmerl),
                 docsh_beam:source_file(Beam) /= false ].

-spec to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh_internal:t()}
         | {error, any(), [erlang:stack_item()]}.
to_internal(Beam) ->
    to_internal(Beam, []).

to_internal(Beam, Opts) ->
    try
        File = case docsh_beam:source_file(Beam) of
                   false -> error(edoc_requires_source);
                   F when is_list(F) -> F
               end,
        {_Mod, EDoc} = edoc:get_doc(File, []),
        [ write(docsh_beam:name(Beam), Tag, OutDir, dispatch(Tag, File, EDoc))
          || {OutDir, Tags} <- [proplists:get_value(debug, Opts)],
             Tag <- Tags ],
        Internal = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
        {ok, Internal}
    catch
        _:R -> {error, R, erlang:get_stacktrace()}
    end.

dispatch(source,    File, _EDoc) ->
    {ok, Content} = file:read_file(File),
    Content;
dispatch(edoc,     _File,  EDoc) -> pp(EDoc);
dispatch(xml,      _File,  EDoc) -> pp(xmerl:export_simple([EDoc], xmerl_xml));
dispatch(html,     _File,  EDoc) -> io_lib:format("~s", [edoc:layout(EDoc)]);
dispatch(flat,     _File,  EDoc) -> pp(xmerl:export_simple([EDoc], docsh_edoc_xmerl_flat));
dispatch(internal, _File,  EDoc) -> pp(xmerl:export_simple([EDoc], docsh_edoc_xmerl));
dispatch(otpsgml,  _File,  EDoc) -> io_lib:format("~s", [xmerl:export_simple([EDoc], xmerl_otpsgml)]);
dispatch(text,     _File,  EDoc) -> pp(xmerl:export_simple([EDoc], xmerl_text)).

write(Mod, Tag, print, Content) ->
    print("\n>>> ~p.~p:\n~p\n", [Mod, Tag, Content]);
write(Mod, Tag, OutDir, Content) ->
    SMod = atom_to_list(Mod),
    STag = atom_to_list(Tag),
    file:write_file(filename:join([OutDir,  SMod ++ "." ++ STag]), Content).

pp(Content) ->
    io_lib:format("~p", [Content]).
