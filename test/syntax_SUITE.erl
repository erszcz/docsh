-module(syntax_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(TESTED, docsh_syntax).

all() ->
    [syntax_to_internal].

syntax_to_internal(_) ->
    {ok, DBeam} = docsh_beam:from_loaded_module(edoc_example),
    ct:pal("~p", [DBeam]),
    ?eq( #{name => edoc_example,
           items =>
           [#{arity => 0,kind => function,name => f,
              signature => <<"-spec f() -> r().\n">>},
            #{arity => 1,kind => type,name => l,
              signature => <<"-type l(A) :: [A].\n">>},
            #{arity => 0,kind => type,name => l,
              signature => <<"-type l() :: list().\n">>},
            #{arity => 0,kind => type,name => r,
              signature => <<"-type r() :: ok.\n">>},
            #{arity => 0,kind => type,name => s,
              signature =>
              <<"-opaque s() :: opaque_type.\n">>},
            #{arity => 1,kind => type,name => t,
              signature => <<"-type t(Arg) :: [Arg].\n">>}]},

         unwrap(?TESTED:to_internal(DBeam)) ).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

unwrap({ok, V}) -> V;
unwrap(Else) -> erlang:error(not_ok, [Else]).
