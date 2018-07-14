-module(rebar3_docsh_prv_SUITE).
-compile([export_all]).

-import(docsh_helpers, [check_precondition/2,
                        sh/1]).

init_per_suite(Config) ->
    [ check_precondition(P, Config) || P <- preconditions() ],
    Config.

preconditions() ->
    [
     { "git in $PATH", fun (_) -> {_, _, <<"usage: git", _/bytes>>} = sh("git --help") end }
    ].

end_per_suite(_Config) ->
    ok.

%%
%% Helpers
%%
