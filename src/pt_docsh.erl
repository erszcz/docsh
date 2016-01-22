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
                 %% TODO: this should be stored in the "ExDc" chunk,
                 %%       but it's probably not possible from within a parse transform
                 %%       and it must be stored somewhere for now
                 embed('__docs', convert(docsh_edoc, docsh_elixir_docs_v1, AST))
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

%% TODO: this suffers from pathological indentosis,
%%       but it's a parse transform trigger...
%%       the fun can't be bound to a variable,
%%       and it doesn't compile as a named function due to calling
%%       local non-existent '__docs'/0
h0() ->
    codegen:gen_function('h',
        fun () ->
            T = case '__docs'() of
                    {elixir_docs_v1, Docs} ->
                        {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                        ModDoc;
                    _ ->
                        <<"Module documentation not found">>
                end,
            io:format("~s~n", [T])
        end).
