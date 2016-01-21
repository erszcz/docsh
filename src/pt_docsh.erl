-module(pt_docsh).
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    File = file(AST),
    print("file: ~s~n", [File]),
    %print("AST: ~p~n", [AST]),
    %print("xmerl doc: ~p~n", [edoc(AST)]),
    print("xml doc: ~s~n", [xml(AST)]),
    AST.

print(Fmt, Args) ->
    io:format(Fmt, Args).

file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.

edoc(AST) ->
    {_Mod, EDoc} = edoc:get_doc(file(AST), []),
    EDoc.

xml(AST) ->
    XML = lists:flatten(xmerl:export_simple([edoc(AST)], xmerl_xml)),
    iolist_to_binary(io_lib:format("~s~n", [XML])).
