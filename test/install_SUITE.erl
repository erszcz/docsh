-module(install_SUITE).
-compile([export_all, nowarn_export_all]).

-import(docsh_helpers, [check_precondition/2,
                        current_git_commit/0,
                        sh/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(b2l(B), binary_to_list(B)).
-define(il2b(IL), iolist_to_binary(IL)).

all() ->
    [docker_linux].

init_per_suite(Config) ->
    [ check_precondition(P, Config) || P <- preconditions() ],
    Config.

preconditions() ->
    [
     { "docker in $PATH", fun (_Config) -> {_, _, <<"Docker", _/bytes>>} = sh("docker -v") end },
     { "git in $PATH", fun (_) -> {_, _, <<"usage: git", _/bytes>>} = sh("git --help") end }
    ].

end_per_suite(_Config) ->
    ok.

%%
%% Config
%%

docsh_repo() ->
    "https://github.com/erszcz/docsh".

%%
%% Tests
%%

docker_linux(_) ->
    %% debug shell commands?
    put(sh_log, true),
    Name = container_name("docsh-linux-"),
    Image = "erlang:20-slim",
    sh(["docker pull ", Image]),
    Args = [which("docker"), "run", "-t", "--rm", "--name", Name, Image, "bash"],
    start_container(Name, Args),
    GitRef = current_git_commit(),
    try
        sh(within_container(Name, clone(docsh_repo()))),
        sh(within_container(Name, checkout(docsh_repo(), GitRef))),
        sh(within_container(Name, install(docsh_repo()))),
        sh(within_container(Name, file_exists("/root/.erlang"))),
        sh(within_container(Name, "cat /root/.erlang")),
        sh(within_container(Name, file_exists("/root/.erlang.d/user_default.erl"))),
        sh(within_container(Name, "cat /root/.erlang.d/user_default.erl")),
        sh(within_container(Name, file_exists("/root/.erlang.d/user_default.beam"))),
        %% "Enabled docsh from: /docsh" is the slogan printed by "~/.erlang".
        %% "docsh" is the string we print in this particular test.
        %% The two get concatenated by erlsh command runner,
        %% hence the strange expected string.
        Version = ?il2b(docsh:version()),
        Expected = <<"Enabled docsh ", Version/bytes, " from: /docsh/_build/default/lib/docsh"
                     "Call h(docsh) for interactive help."
                     ""
                     "docsh">>,
        {_, _, Expected} = sh(within_container(Name, docsh_works()))
    after
        sh("docker stop " ++ Name)
    end.

%%
%% Helpers
%%

container_name(Prefix) ->
    RawRandomBytes = crypto:strong_rand_bytes(9),
    Base64 = base64:encode(RawRandomBytes),
    DockerCompliant = re:replace(Base64, <<"[^a-zA-Z0-9_.-]">>, <<"x">>, [global]),
    ?b2l(?il2b([Prefix, DockerCompliant])).

which(Command) ->
    {done, 0, BPath} = sh("which " ++ Command),
    ?b2l(BPath).

start_container(Name, Args) ->
    Fdlink = erlsh:fdlink_executable(),
    _ContainerPort = erlang:open_port({spawn_executable, Fdlink}, [stream, exit_status, {args, Args}]),
    wait_for(fun () -> is_container_running(Name) end),
    setup_container_system(Name).

setup_container_system(Name) ->
    sh(within_container(Name, "apt-get update")),
    Packages = ["ca-certificates", "git"],
    sh(within_container(Name, ["apt-get install --no-install-recommends --yes ",
                               string:join(Packages, " ")])).

wait_for(Predicate) ->
    wait_for(Predicate, 10 * 1000).

wait_for(_Predicate, Timeout) when Timeout < 0 ->
    erlang:error(timeout);
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

clone(Repo) ->
    ["git clone ", Repo].

checkout(Repo, GitRef) ->
    RepoDir = filename:basename(Repo),
    ["bash -c ", quote(["cd ", RepoDir, "; git checkout ", GitRef])].

quote(Text) ->
    ["\"", Text, "\""].

within_container(Name, Command) ->
    ["docker exec ", Name, " ", Command].

install(Repo) ->
    RepoDir = filename:basename(Repo),
    ["bash -c ", quote(["yes | ", RepoDir, "/install.sh"])].

file_exists(File) ->
    ["test -f ", File].

docsh_works() ->
    ["erl -eval 'erlang:display(docsh:module_info(module)).' -s erlang halt"].
