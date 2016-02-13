-module(docsh_rt).
-compile([debug_info,
          {inline, [guard_no_docs/2,
                    guard_not_supported/2]}]).

-export([h/1,
         h/3]).

-import(docsh_lib, [print/2]).

-define(il2b(IOList), iolist_to_binary(IOList)).

h(Mod) ->
    F = fun (Docs) ->
                {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                ModDoc
        end,
    guard_no_docs(Mod, F).

h(Mod, Fun, Arity) ->
    F = fun (Docs) ->
                {docs, FunDocs} = lists:keyfind(docs, 1, Docs),
                FA = {Fun, Arity},
                %% TODO: fragile
                {FA,_,_,_,Doc} = lists:keyfind(FA, 1, FunDocs),
                Doc
        end,
    guard_no_docs(Mod, F).

guard_no_docs(Mod, Fun) ->
    T = try
            guard_not_supported(Fun, Mod:'__docs'())
        catch
            error:undef ->
                <<"Module documentation not found">>;
            E:R ->
                ?il2b([<<"Internal error: ">>,
                       io_lib:format("~p:~p", [E, R])])
        end,
    io:format("~s~n", [T]).

guard_not_supported(Fun, {elixir_docs_v1, Docs}) ->
    Fun(Docs);
guard_not_supported(_, _) ->
    <<"Documentation format not supported">>.
