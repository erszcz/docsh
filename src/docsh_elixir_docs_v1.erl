-module(docsh_elixir_docs_v1).
-behaviour(docsh_to).

-export([from_internal/1]).

-export_type([external/0]).

-type external() :: {elixir_docs_v1, [{atom(), any()}]}.

-import(docsh_lib, [get/2, get/3]).

-spec from_internal(docsh:internal()) -> any().
from_internal(Internal) ->
    Intermediate = [ Out || In <- Internal, Out <- [do(In)], Out /= ignore ],
    {_, ModDoc, Rest} = lists:keytake(moduledoc, 1, Intermediate),
    Docs = proplists:get_all_values(docs, Rest),
    Types = proplists:get_all_values(types, Rest),
    {elixir_docs_v1, [{docs, Docs} || Docs /= [] ] ++
                     [{types, Types} || Types /= [] ] ++
                     [ModDoc]}.

%% TODO: `x`s below are only placeholders - find out what should be there
do({module, Info}) ->
    {moduledoc, {x, get(description, Info, <<"(description missing)">>)}};
do({{type, NameArity}, {description, Desc}}) ->
    %% TODO: this is not complient with Elixir docs format! make it so!
    {types, {NameArity, Desc}};
do({{function, NameArity}, {_, _, _, _, {description, D}}}) ->
    {docs, {NameArity,
            x, def,
            x,
            D}}.
