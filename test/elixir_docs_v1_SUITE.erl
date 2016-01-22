-module(elixir_docs_v1_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_elixir_docs_v1).

all() ->
    [internal_to_elixir_docs_v1].

internal_to_elixir_docs_v1(_) ->
    Docs = ?TESTED:from_internal(internal()),
    ?assertEqual(elixir_docs_v1, element(1, Docs)),
    %% TODO: `x` chars below are placeholders - insert sensible values into the example
    ?eq([{docs, [{{f,0},
                  x, def,
                  x,
                  <<"Doc for f/0.">>}]},
         {moduledoc,
          {x, <<"Top-level module doc.">>}}],
        element(2, Docs)).

internal() ->
    [{module,[{name,edoc_example},{description,<<"Top-level module doc.">>}]},
     {function,[{name,f},
                {arity,0},
                {exported,true},
                {label,<<"f-0">>},
                {description,<<"Doc for f/0.">>}]}].
