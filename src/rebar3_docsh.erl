%% @doc Store modules' documentation in Docs chunks according to EEP-48.
%%
%% Add the following code to your project's `rebar.config':
%%
%% ```
%% {plugins,
%%  [
%%   {rebar3_docsh, "0.7.2", {pkg, docsh}}
%%  ]}.
%%
%% {provider_hooks,
%%  [
%%   {post, [{compile, {docsh, compile}}]}
%%  ]}.
%% '''
%%
%% @end
-module(rebar3_docsh).

-behaviour(provider).
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).
-define(SHORT_DESC, "Store modules' documentation in Docs chunks according to EEP-48").
-define(DESC, "Store modules' documentation in Docs chunks according to EEP-48.\n"
              "This exposes Erlang module documentation to Elixir and other BEAM languages.\n"
              "Use https://github.com/erszcz/docsh to access your docs in an Erlang shell.\n").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    POpts = [
             {name, ?PROVIDER},             % The 'user friendly' name of the task
             {namespace, docsh},
             {module, ?MODULE},             % The module implementation of the task
             {bare, true},                  % The task can be run by the user, always true
             {deps, ?DEPS},                 % The list of dependencies
             {opts, []},                    % list of options understood by the plugin
             {short_desc, ?SHORT_DESC},
             {desc, ?DESC}
            ],
    Provider = providers:create(POpts),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [ process_app(State, App) || App <- Apps ],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    docsh_lib:format_error(Reason).

%% ===================================================================
%% Helpers
%% ===================================================================

-spec process_app(rebar_state:t(), rebar_app_info:t()) -> ok.
process_app(State, App) ->
    BEAMs = app_beam_files(App),
    [ process_beam(State, B) || B <- BEAMs ],
    ok.

-spec app_beam_files(rebar_app_info:t()) -> [file:filename()].
app_beam_files(App) ->
    EbinDir = rebar_app_info:ebin_dir(App),
    filelib:wildcard(filename:join([EbinDir, "*.beam"])).

-spec process_beam(rebar_state:t(), file:filename()) -> ok.
process_beam(_State, BeamFile) ->
    case docsh_lib:has_docs(BeamFile) of
        true ->
            ok;
        false ->
            {ok, B} = docsh_beam:from_beam_file(BeamFile),
            {ok, Docs, Warnings} = docsh_lib:make_docs(B),
            print_warnings(docsh_beam:name(B), Warnings),
            DocsChunk = make_docs_chunk(Docs),
            {ok, NewBeam} = add_chunks(BeamFile, [DocsChunk]),
            ok = file:write_file(BeamFile, NewBeam)
    end.

print_warnings(Name, Warnings) ->
    [ docsh_lib:print("~s", [docsh_lib:format_error({W, Name})]) || W <- Warnings ].

make_docs_chunk(Docs) ->
    {"Docs", term_to_binary(Docs, [compressed])}.

add_chunks(BeamFile, NewChunks) ->
    {ok, _, OldChunks} = beam_lib:all_chunks(BeamFile),
    {ok, _NewBEAM} = beam_lib:build_module(OldChunks ++ NewChunks).
