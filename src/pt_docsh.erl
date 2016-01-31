-module(pt_docsh).
-export([parse_transform/2]).

-compile([{parse_transform, parse_trans_codegen}]).

-import(docsh_lib, [print/2]).

-define(il2b(IOList), iolist_to_binary(IOList)).

-spec parse_transform(AST, [compile:option()]) -> AST when
      AST :: [erl_parse:abstract_form()].
parse_transform(AST, _Options) ->
    {Attrs, Rest} = lists:splitwith(fun is_not_spec/1, AST),
    docsh_lib:debug(syntax, "~p~n", [AST]),
    ASTAfter = (Attrs ++
                [export([{h, 0}, {h, 2}]),
                 h0(),
                 h2(),
                 %% TODO: this should be stored in the "ExDc" chunk,
                 %%       but it's probably not possible from within a parse transform
                 %%       and it must be stored somewhere for now
                 embed('__docs', convert(docsh_edoc, docsh_elixir_docs_v1, AST))
                 | Rest]),
    %print("after: ~p~n", [ASTAfter]),
    ASTAfter.

export(Functions) ->
    {attribute, 1, export, Functions}.

convert(From, To, AST) ->
    Internal = From:to_internal(file(AST)),
    To:from_internal(Internal).

file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.

embed(EmbeddedName, Docs) ->
    %% codegen:gen_function is expanded by a parse tree transformation,
    %% see https://github.com/uwiger/parse_trans/
    codegen:gen_function(EmbeddedName, fun () -> {'$var', Docs} end).

is_not_spec({attribute,_,spec,_}) -> false;
is_not_spec(_) -> true.

h0() ->
    H0 = codegen:exprs
        (fun () ->
                 fun ({elixir_docs_v1, Docs}) ->
                         {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                         ModDoc;
                     (_) ->
                         <<"Documentation format unrecognized">>
                 end
         end),
    %% {'$form', F} parameter F has to be a single form,
    %% therefore we strip the outer list.
    [G] = guard(H0),
    codegen:gen_function ('h', fun () -> {'$form', G} end).

h2() ->
    H2 = codegen:exprs
        (fun (Fun, Arity) ->
                 fun ({elixir_docs_v1, Docs}) ->
                         {docs, FunDocs} = lists:keyfind(docs, 1, Docs),
                         FA = {Fun, Arity},
                         %% TODO: fragile
                         {FA,_,_,_,Doc} = lists:keyfind(FA, 1, FunDocs),
                         Doc;
                     (_) ->
                         <<"Documentation format unrecognized">>
                 end
         end),
    [G] = guard(H2),
    codegen:gen_function ('h', fun (Fun, Arity) -> {'$form', G} end).

guard([F]) ->
    %% codegen:exprs is expanded by a parse tree transformation,
    %% see https://github.com/uwiger/parse_trans/
    codegen:exprs
        (fun () ->
                 %% The return value has to be a single value in a list,
                 %% but T = ..., io:format(...) would be a 2-element list.
                 %% Therefore we use a IIFE to return a single value.
                 (fun () ->
                          T = try
                                  ({'$form', F})('__docs'())
                              catch
                                  error:undef ->
                                      <<"Module documentation not found">>;
                                  E:R ->
                                      ?il2b([<<"Internal error: ">>,
                                             io_lib:format("~p:~p", [E, R])])
                              end,
                          io:format("~s~n", [T])
                  end)()
         end).
