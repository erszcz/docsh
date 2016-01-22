-module(pt_docsh).
-export([parse_transform/2]).
-import(docsh_lib, [print/2]).

-compile([{parse_transform, parse_trans_codegen}]).

parse_transform(AST, _Options) ->
    print("before: ~p~n", [AST]),
    {Attrs, Rest} = lists:partition(fun is_attribute/1, AST),
    ASTAfter = (Attrs ++
                [%% TODO: adding this export dynamically breaks compilation,
                 %%       see also include/pt_docsh.hrl
                 %export([{h,0}]),
                 h0(),
                 embed('__elixir_docs_v1', convert(docsh_edoc, docsh_elixir_docs_v1, AST))
                 | Rest]),
    print("after: ~p~n", [ASTAfter]),
    ASTAfter.

convert(From, To, AST) ->
    Internal = From:to_internal(file(AST)),
    To:from_internal(Internal).

file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.

export(FunArity) ->
    {attribute,1,export,[FunArity]}.

embed(EmbeddedName, Docs) ->
    codegen:gen_function(EmbeddedName, fun () -> {'$var', Docs} end).

is_attribute({attribute,_,_,_}) -> true;
is_attribute(_) -> false.

h0() ->
    codegen:gen_function('h', fun () ->
                                      {elixir_docs_v1, Docs} = '__elixir_docs_v1'(),
                                      io:format("~p~n", [Docs])
                              end).
