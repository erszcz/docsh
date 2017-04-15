-module(install_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(b2l(B), binary_to_list(B)).
-define(il2b(IL), iolist_to_binary(IL)).

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
    Name = container_name("docker-linux-"),
    Fdlink = erlsh:fdlink_executable(),
    Args = [which("docker"), "run", "-t", "--rm", "--name", Name, "erlang:19-slim", "bash"],
    ContainerPort = erlang:open_port({spawn_executable, Fdlink}, [stream, exit_status, {args, Args}]),
    try
        ct:fail("not implemented yet")
    after
        sh("docker stop " ++ Name)
    end.

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

container_name(Prefix) ->
    ?b2l(?il2b([Prefix, base64:encode(crypto:rand_bytes(9))])).

which(Command) ->
    {done, 0, BPath} = sh("which " ++ Command),
    ?b2l(BPath).

sh(Command) ->
    {done, 0, _} = erlsh:oneliner(Command).
