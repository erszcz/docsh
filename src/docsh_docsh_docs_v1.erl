-module(docsh_docsh_docs_v1).

-behaviour(docsh_format).
-export([lookup/3]).

-behaviour(docsh_writer).
-export([from_internal/1]).

-type t() :: {docsh_docs_v1, [{docs | specs | types | moduledoc, any()}]}.

-type key() :: module() | mfa() | {module(), atom(), any}.
-type item_kind() :: moduledoc | doc | spec | type.

-import(docsh_lib, [get/2, get/3]).

-define(a2b(A), atom_to_binary(A, utf8)).
-define(a2l(A), atom_to_list(A)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

%%
%%' Public
%%

-spec lookup(Docs, key(), [item_kind()]) -> R when
      Docs :: docsh_docsh_docs_v1:t()
            | docsh_format:t(),
      R :: {ok, binary()} | {not_found, docsh_format:error_message()}.
lookup({docsh_docs_v1, Docs}, Key, Opts) ->
    case fetch_features(Docs, Key, Opts) of
        [] ->
            {not_found, ?il2b(no_features(Key, Opts))};
        Features ->
            {ok, ?il2b(format_features(Features, key_to_arity(Key), Opts))}
    end;
lookup(Format, _Key, _Opts) ->
    {not_found, ?il2b(unrecognized_format(Format))}.

-spec from_internal(docsh_internal:t()) -> t().
from_internal(Internal) ->
    Intermediate = [ Out || In <- Internal, Out <- [do(In)], Out /= ignore ],
    {_, ModDoc, Rest} = lists:keytake(moduledoc, 1, Intermediate),
    Docs = proplists:get_all_values(docs, Rest),
    Specs = proplists:get_all_values(specs, Rest),
    Types = proplists:get_all_values(types, Rest),
    {docsh_docs_v1, [{docs, Docs} || Docs /= [] ] ++
                    [{specs, Specs} || Specs /= [] ] ++
                    [{types, Types} || Types /= [] ] ++
                    [ModDoc]}.

%%.
%%' Internal
%%

%% TODO: `x's below are only placeholders - find out what should be there
do({module, Info}) ->
    {moduledoc, {x, get(description, Info, <<"Documentation for the module is not available.\n">>)}};
do({{type, NameArity}, {description, Desc}}) ->
    %% TODO: this is not compliant with Elixir docs format! make it so!
    {types, {NameArity, Desc}};
do({{spec, NameArity}, {description, Desc}}) ->
    %% TODO: this is not compliant with Elixir docs format! make it so!
    {specs, {NameArity, Desc}};
do({{function, NameArity}, {_, _, _, _, {description, D}}}) ->
    {docs, {NameArity,
            x, def,
            x,
            if
                D == undefined -> <<"Documentation is not available.\n\n">>;
                D /= undefined -> D
            end}}.

-spec key_to_arity(key()) -> any | arity().
key_to_arity(M) when is_atom(M) -> any;
key_to_arity({_,_,A}) -> A.

fetch_features(Docs, Key, Opts) ->
    FlatDocs = flatten_docs(Docs),
    Features = filter_features(FlatDocs, Key, Opts),
    Arities = find_arities(Features),
    generate_headers(Key, Arities) ++ Features.

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

filter_features(FlatDocs, Mod, [moduledoc]) when is_atom(Mod) ->
    {moduledoc, {_, Doc}} = lists:keyfind(moduledoc, 1, FlatDocs),
    [{moduledoc, Mod, Doc}];
filter_features(FlatDocs, Key, FeatureKinds) ->
    {_Mod, Name, Arity} = case Key of
                              M when is_atom(M) -> {M, any, any};
                              {M, N, A} -> {M, N, A}
                          end,
    [ Feature || {Kind, ActualName, ActualArity, _} = Feature <- FlatDocs,
                 lists:member(Kind, FeatureKinds),
                 does_name_match(Name, ActualName),
                 does_arity_match(Arity, ActualArity) ].

does_name_match(any, _) -> true;
does_name_match(N, N) -> true;
does_name_match(_, _) -> false.

does_arity_match(any, _) -> true;
does_arity_match(A, A) -> true;
does_arity_match(_, _) -> false.

find_arities(Features) ->
    lists:usort([ A || {Kind, _, A, _} <- Features,
                       Kind == doc orelse Kind == spec ]).

generate_headers(Mod, _Arities) when is_atom(Mod) ->
    [];
generate_headers({Mod, Name, _}, Arities) ->
    [ header(Mod, Name, Arity) || Arity <- Arities ].

header(M, F, A) -> {header, M, F, A}.

format_features(Features, any, [type]) ->
    [ format_feature(F) || F <- Features ];
format_features(Features, any, Opts) ->
    [ format_features(FeatureGroup, Arity, Opts)
      || {Arity, FeatureGroup} <- sort_by_arity(group_by_arity(Features)) ];
format_features(Features, Arity, _Opts) when is_integer(Arity) ->
    [ format_feature(F) || F <- sort_features(Features) ].

sort_features(Features) ->
    Order = [moduledoc, type, header, spec, doc],
    [ F || Key <- Order, F <- [lists:keyfind(Key, 1, Features)], F /= false ].

format_feature({moduledoc, Mod, Doc}) ->
    io_lib:format("\n# ~s~n~n~s~n", [Mod, Doc]);
format_feature({header, M, F, A}) ->
    [$\n, format_mfa(M, F, A), "\n\n"];
format_feature({_Kind, _, _, Doc}) ->
    [Doc, $\n].

no_features(Key, Opts) ->
    io_lib:format("\ndocsh: no ~ts for ~ts\n\n",
                  [format_kinds(Opts), format_key(Key)]).

unrecognized_format(Format) ->
    io_lib:format("\ndocsh: ~ts is not recognized by ~ts - please report at "
                  "https://github.com/erszcz/docsh\n\n",
                  [element(1, Format), ?MODULE]).

format_kinds(Kinds) ->
    string:join([ ?a2l(K) || K <- Kinds ], "/").

format_key(M) when is_atom(M) -> ?a2b(M);
format_key({M, F, A}) -> format_mfa(M, F, A).

format_mfa(M, F, A) ->
    [?a2b(M), $:, ?a2b(F), $/, case A of any -> $*; _ -> ?i2b(A) end].

group_by_arity(Features) ->
    dict:to_list(docsh_lib:group_by(fun feature_arity/1, Features)).

feature_arity({moduledoc, _, _}) -> 0;
feature_arity({header, _, _, A}) -> A;
feature_arity({doc, _, A, _}) -> A;
feature_arity({spec, _, A, _}) -> A;
feature_arity({type, _, A, _}) -> A.

sort_by_arity(FeatureGroups) ->
    lists:sort(FeatureGroups).

%%. vim: foldmethod=marker foldmarker=%%',%%.
