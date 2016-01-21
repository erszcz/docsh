-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [help,
     spec].

help(_) ->
    ?eq(<<"Doc for f/0.">>,
        ?TESTED:'__h'()),
    ?assert(erlang:function_exported(?TESTED, h, 0)).

spec(_) ->
    ?eq(<<"-spec f() -> ok.">>,
        ?TESTED:'__spec'()),
    ?assert(erlang:function_exported(?TESTED, spec, 0)).
