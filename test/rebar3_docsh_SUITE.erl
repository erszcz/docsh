-module(rebar3_docsh_SUITE).
-compile([export_all, nowarn_export_all]).

-import(docsh_helpers, [check_precondition/2,
                        current_git_commit/0,
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

all() ->
    [{group, main}].

groups() ->
    [{main, [sequence],
      [
       rebar3_docsh_compiles_in_the_docs_chunk,
       rebar3_docsh_handles_present_docs_chunk
      ]}].

%%
%% Config
%%

docsh_repo() ->
    "https://github.com/erszcz/docsh".

recon_repo() ->
    "https://github.com/erszcz/recon".

%%
%% Tests
%%

rebar3_docsh_compiles_in_the_docs_chunk(_) ->
    put(sh_log, true),
    %% given
    AppName = "recon",
    sh(clone(recon_repo())),
    get_docsh_plugin(AppName, docsh_repo(), current_git_commit()),
    %% when
    {ok, ProjectDir} = compile(AppName),
    %% then all modules have the "Docs" chunk
    {ok, Modules} = app_modules(AppName, ProjectDir),
    ModuleDocs = [ begin
                       %% Not using docsh here so we're 100% sure it doesn't make docs_v1 on demand.
                       BeamFile = code:which(M),
                       {ok, {_Mod, [{"Docs", BDocs}]}} = beam_lib:chunks(BeamFile, ["Docs"]),
                       {ok, erlang:binary_to_term(BDocs)}
                   end || M <- Modules ],
    %ct:pal("~s module docs:\n~p", [AppName, ModuleDocs]),
    ok.

rebar3_docsh_handles_present_docs_chunk(_) ->
    %% given the previously cloned repo (these tests run in sequence!)
    AppName = "recon",
    %% when compiling / then we should exit without `docs_present` error
    {ok, _ProjectDir} = compile(AppName),
    ok.

%%
%% Helpers
%%

quote(Text) ->
    ["\"", Text, "\""].

clone(Repo) ->
    ["git clone ", Repo].

clone(Repo, Target) ->
    ["git clone ", Repo, " ", Target].

compile(Project) ->
    {ok, Dir} = file:get_cwd(),
    ProjectDir = filename:join([Dir, Project]),
    try
        ok = file:set_cwd(ProjectDir),
        rebar_agent:start_link(rebar_state:new()),
        ok = rebar_agent:do(compile),
        {ok, ProjectDir}
    after
        ok = file:set_cwd(Dir)
    end.

get_docsh_plugin(AppName, DocshRepo, Ref) ->
    DocshPlugin = filename:join([AppName, "_checkouts", "rebar3_docsh"]),
    ok = filelib:ensure_dir(DocshPlugin),
    sh(clone(DocshRepo, DocshPlugin)),
    sh("cd " ++ DocshPlugin ++ " && git checkout " ++ Ref).

app_modules(AppName, ProjectDir) ->
    AppFile = filename:join([ProjectDir, "_build", "default", "lib", AppName, "ebin", AppName ++ ".app"]),
    {ok, AppSpec} = file:consult(AppFile),
    [{application, recon, AppProps}] = AppSpec,
    {modules, Modules} = lists:keyfind(modules, 1, AppProps),
    {ok, Modules}.
