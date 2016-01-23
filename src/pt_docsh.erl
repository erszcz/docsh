-module(pt_docsh).
-export([parse_transform/2]).

-compile([{parse_transform, parse_trans_codegen}]).

-import(docsh_lib, [print/2]).

-define(il2b(IOList), iolist_to_binary(IOList)).

parse_transform(AST, _Options) ->
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
    %print("after: ~p~n", [ASTAfter]),
    ASTAfter.

convert(From, To, AST) ->
    Internal = From:to_internal(file(AST)),
    To:from_internal(Internal).

file(AST) ->
    {_,_,file,{File,_}} = lists:keyfind(file, 3, AST),
    File.

embed(EmbeddedName, Docs) ->
    codegen:gen_function(EmbeddedName, fun () -> {'$var', Docs} end).

is_attribute({attribute,_,_,_}) -> true;
is_attribute(_) -> false.

%% TODO: this variant doesn't work due to a bug thrown from rebar.
%%       am I doing something wrong?
h0() ->
    guard('h', codegen:exprs(fun h0_prototype/0)).

h0_prototype() ->
    fun ({elixir_docs_v1, Docs}) ->
            {_, ModDoc} = proplists:get_value(moduledoc, Docs),
            ModDoc;
        (_) ->
            <<"Documentation format unrecognized">>
    end.

guard(Name, [F]) ->
    codegen:gen_function(Name,
        fun () ->
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
        end).
