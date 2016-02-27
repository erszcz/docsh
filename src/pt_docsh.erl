-module(pt_docsh).
-export([parse_transform/2]).

-compile([{parse_transform, parse_trans_codegen}]).

-import(docsh_lib, [convert/3,
                    print/2]).

-define(il2b(IOList), iolist_to_binary(IOList)).

-spec parse_transform(AST, [compile:option()]) -> AST when
      AST :: [erl_parse:abstract_form()].
parse_transform(AST, _Options) ->
    {Attrs, Rest} = lists:splitwith(fun is_not_spec/1, AST),
    docsh_lib:debug(syntax, "~p~n", [AST]),
    ASTAfter = (Attrs ++
                [
                 %% TODO: code in docsh_rt expects __docs/0 to be exported
                 export([{'__docs', 0}]),
                 %% TODO: this should be stored in the "ExDc" chunk,
                 %%       but it's probably not possible from within a parse transform
                 %%       and it must be stored somewhere for now
                 embed('__docs', convert([docsh_edoc, docsh_syntax], docsh_elixir_docs_v1, AST))
                 | Rest]),
    %print("after: ~p~n", [ASTAfter]),
    ASTAfter.

export(Functions) ->
    {attribute, 1, export, Functions}.

embed(EmbeddedName, Docs) ->
    %% codegen:gen_function is expanded by a parse tree transformation,
    %% see https://github.com/uwiger/parse_trans/
    codegen:gen_function(EmbeddedName, fun () -> {'$var', Docs} end).

is_not_spec({attribute,_,spec,_}) -> false;
is_not_spec(_) -> true.
