-module(docsh_embeddable).
-compile([debug_info,
          {inline, [get_elixir_docs_v1/1,
                    do_with_docs/3,
                    do_with_supported/3,
                    types/1]}]).

-export([h/1,
         h/4]).

-type fname() :: atom().

-import(docsh_lib, [print/2]).

-define(a2b(A), atom_to_binary(A, utf8)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

-spec h(module()) -> ok.
h(Mod) ->
    F = fun (Docs, _) ->
                {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                %% TODO: work on the printout format in cases
                %%       of unavailable docs
                io_lib:format("## Description~n~n~s~n"
                              "## Types~n~s~n",
                              [ModDoc, types(Docs)])
        end,
    do_with_docs(Mod, F, []).

-spec h(module(), fname(), arity(), [term()]) -> ok.
h(Mod, Fun, Arity, Opts) ->
    F = fun (Docs, Opts) ->
                case fetch_function({Fun, Arity}, Docs) of
                    {no_docs, no_specs} ->
                        error({no_docs, <<"neither doc comments nor specs found">>});
                    {{doc, not_found}, {spec, not_found}} ->
                        error({no_docs, <<(mfa(Mod, Fun, Arity))/bytes, " not found">>});
                    {{doc, Doc}, {spec, Spec}} ->
                        format([ {doc, Doc}   || proplists:is_defined(doc, Opts) ] ++
                               [ {spec, Spec} || proplists:is_defined(spec, Opts) ])
                end
        end,
    do_with_docs(Mod, F, Opts).

do_with_docs(Mod, Fun, Opts) ->
    T = try
            do_with_supported(Fun, get_elixir_docs_v1(Mod), Opts)
        catch
            error:{no_docs, R} ->
                <<"Docs missing:", R/bytes>>;
            _:R ->
                ?il2b([<<"docsh error: ">>,
                       io_lib:format("~p\n~p\n", [R, erlang:get_stacktrace()])])
        end,
    io:format("~s~n", [T]).

do_with_supported(Fun, {elixir_docs_v1, Docs}, Opts) ->
    Fun(Docs, Opts);
do_with_supported(_, _, _) ->
    <<"Documentation format not supported">>.

fetch_function(_, _) ->
    not_implemented_yet.

fetch_doc(FunArity, Docs) ->
    {doc, case lists:keyfind(FunArity, 1, Docs) of
              false -> not_found;
              {FunArity, _, _, Doc} -> Doc
          end}.

fetch_spec(FunArity, Specs) ->
    {spec, case lists:keyfind(FunArity, 1, Specs) of
               false -> not_found;
               {FunArity, Spec} -> Spec
           end}.

format(_) ->
    not_implemented_yet.

types(Docs) ->
    Types = proplists:get_value(types, Docs, []),
    [ ["\n", Desc] || {{_Name, _Arity}, Desc} <- Types ].

get_elixir_docs_v1(Mod) ->
    BEAMFile = code:which(Mod),
    case beam_lib:chunks(BEAMFile, ["ExDc"]) of
        {ok, {Mod, [{"ExDc", BExDc}]}} ->
            erlang:binary_to_term(BExDc);
        {error, _, {missing_chunk, _, _}} ->
            error({no_docs, <<"no ExDc chunk">>})
    end.

mfa(M, F, A) ->
    <<>>.
    %<<?a2b(M)/bytes, ":", ?a2b(F)/bytes, "/", ?i2b(A)/bytes>>.
