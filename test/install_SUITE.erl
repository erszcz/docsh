-module(install_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [docker_linux].

init_per_suite(Config) ->
    [ check(P, Config) || P <- prerequisites() ],
    Config.

prerequisites() ->
    [ 
     { "docker in $PATH?",
       fun (_Config) ->
               {done, 0, <<"Docker", _/bytes>>} = erlsh:oneliner("docker -v")
       end}
    ].

end_per_suite(_Config) ->
    ok.

%%
%% Tests
%%

docker_linux(_) ->
    error("not implemented yet").

%%
%% Helpers
%%

check({Name, P}, Config) ->
    try
        P(Config),
        ok
    catch _:Reason ->
        ct:fail("~ts failed: ~p", [Name, Reason])
    end.
