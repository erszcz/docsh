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
    Name = container_name("docsh-linux-"),
    Args = [which("docker"), "run", "-t", "--rm", "--name", Name, "erlang:19-slim", "bash"],
    start_container(Name, Args),
    try
        sh(["docker exec ", Name, " echo a"]),
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
    ?b2l(?il2b([Prefix, base64:encode(crypto:strong_rand_bytes(9))])).

which(Command) ->
    {done, 0, BPath} = sh("which " ++ Command),
    ?b2l(BPath).

sh(Command) when is_binary(Command) -> sh([Command]);
sh(Command) ->
    {done, 0, _} = erlsh:oneliner(?b2l(?il2b(Command))).

start_container(Name, Args) ->
    Fdlink = erlsh:fdlink_executable(),
    _ContainerPort = erlang:open_port({spawn_executable, Fdlink}, [stream, exit_status, {args, Args}]),
    wait_for(fun () -> is_container_running(Name) end).

wait_for(Predicate) ->
    wait_for(Predicate, 5000).

wait_for(_Predicate, Timeout) when Timeout < 0 ->
    error(timeout);
wait_for( Predicate, Timeout) ->
    case Predicate() of
        true -> ok;
        false ->
            timer:sleep(100),
            wait_for(Predicate, Timeout - 100)
    end.

is_container_running(Name) ->
    try
        sh(["docker ps | grep ", Name, " | grep Up"]),
        true
    catch _:_ -> false end.
