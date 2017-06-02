-module(docsh_embeddable).

%% TODO: As of now the name of this module is a misnomer.
%%       Helper embedding is not currently supported
%%       and the code here wouldn't be embeddable either way.

-export([get_docs/1, get_docs/4]).

-export_type([function_name/0,
              type_name/0]).

-type function_name() :: atom().
-type type_name() :: atom().

-define(a2b(A), atom_to_binary(A, utf8)).
-define(a2l(A), atom_to_list(A)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

-spec get_docs(module()) -> binary().
get_docs(Mod) ->
    F = fun (Docs, _) ->
                {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                %% TODO: work on the printout format in cases
                %%       of unavailable docs
                io_lib:format("\n# Module ~s~n~n"
                              "## Description~n~n~s~n"
                              "## Types~n~s~n",
                              [Mod, ModDoc, types(Docs)])
        end,
    ?il2b([do_with_docs(Mod, F, [])]).

-spec get_docs(module(), function_name(), any | arity(), [term()]) -> binary().
get_docs(Mod, Fun, Arity, Opts) ->
    case fetch_features(Mod, Fun, Arity, Opts) of
        [] -> no_features(Mod, Fun, Arity, Opts);
        Features ->
            ?il2b(format_features(Features, Arity, Opts))
    end.

fetch_features(Mod, Fun, Arity, Opts0) ->
    F = fun (Docs, Opts) ->
                FlatDocs = flatten_docs(Docs),
                Features = filter_features(FlatDocs, Fun, Arity, Opts),
                Arities = find_arities(Features),
                generate_headers(Mod, Fun, Arities) ++ Features
        end,
    do_with_docs(Mod, F, Opts0).

flatten_docs(Docs) ->
    F = fun ({moduledoc, _} = ModDoc) -> [ModDoc];
            ({docs, Functions}) ->
                [ {doc, Fun, Arity, Doc}
                  %% TODO: ultimately, we should use all these fields
                  || {{Fun, Arity}, _, _, _, Doc} <- Functions ];
            ({Kind, Functions}) ->
                [ {map_kind(Kind), Fun, Arity, Doc}
                  || {{Fun, Arity}, Doc} <- Functions ]
        end,
    lists:flatmap(F, Docs).

map_kind(docs) -> doc;
map_kind(specs) -> spec;
map_kind(types) -> type.

filter_features(FlatDocs, Fun, Arity, FeatureKinds) ->
    [ Feature || {Kind, ActualFun, ActualArity, _} = Feature <- FlatDocs,
                 Fun =:= ActualFun,
                 lists:member(Kind, FeatureKinds),
                 does_arity_match(Arity, ActualArity) ].

does_arity_match(any, _) -> true;
does_arity_match(A, A) -> true;
does_arity_match(_, _) -> false.

find_arities(Features) ->
    lists:usort([ A || {Kind, _, A, _} <- Features,
                       Kind == doc orelse Kind == spec ]).

generate_headers(Mod, Fun, Arities) ->
    [ header(Mod, Fun, Arity) || Arity <- Arities ].

header(M, F, A) -> {header, M, F, A}.

format_features(Features, any, Opts) ->
    [ format_features(FeatureGroup, Arity, Opts)
      || {Arity, FeatureGroup} <- sort_by_arity(group_by_arity(Features)) ];
format_features(Features, Arity, _Opts) when is_integer(Arity) ->
    [ format_feature(F) || F <- sort_features(Features) ].

sort_features(Features) ->
    Order = [moduledoc, type, header, spec, doc],
    [ F || Key <- Order, F <- [lists:keyfind(Key, 1, Features)], F /= false ].

format_feature({moduledoc, Doc}) -> Doc;
format_feature({header, M, F, A}) ->
    [$\n, format_mfa(M, F, A), "\n\n"];
format_feature({Kind, _, _, Doc})
  when Kind =:= doc;
       Kind =:= spec ->
    [Doc, $\n];
format_feature({type, _, _, Doc})  ->
    [$\n, Doc, $\n].

no_features(Mod, Fun, Arity, Opts) ->
    docsh_lib:print("\ndocsh: no ~ts for ~ts\n\n",
                    [format_kinds(Opts), format_mfa(Mod, Fun, Arity)]).

format_kinds(Kinds) ->
    string:join([ ?a2l(K) || K <- Kinds ], "/").

format_mfa(M, F, A) ->
    [?a2b(M), $:, ?a2b(F), $/, case A of any -> $*; _ -> ?i2b(A) end].

do_with_docs(Mod, Fun, Opts) ->
  do_with_supported(Fun, get_elixir_docs_v1(Mod), Opts).

do_with_supported(Fun, {elixir_docs_v1, Docs}, Opts) ->
    Fun(Docs, Opts);
do_with_supported(_, _, _) ->
    <<"Documentation format not supported">>.

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

group_by_arity(Features) ->
    dict:to_list(group_by(fun feature_arity/1, Features)).

feature_arity({moduledoc, _}) -> 0;
feature_arity({header, _, _, A}) -> A;
feature_arity({doc, _, A, _}) -> A;
feature_arity({spec, _, A, _}) -> A;
feature_arity({type, _, A, _}) -> A.

group_by(F, L) ->
    lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end,
                dict:new(), [ {F(X), X} || X <- L ]).

sort_by_arity(FeatureGroups) ->
    lists:sort(FeatureGroups).
