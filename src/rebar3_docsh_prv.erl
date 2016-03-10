-module('rebar3_docsh_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).
-define(SHORT_DESC, "Provide embedded module docs for easy shell access").
-define(DESC, "Provide embedded module docs for easy shell access.\n"
              "Make sure {erl_opts, [{core_transform, ct_docsh}]} is in your rebar.config\n"
              "or that you do\n"
              "\n"
              "    -include_lib(\"docsh/include/docsh.hrl\").\n"
              "\n"
              "in every module into which you want to embed documentation.\n").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, docsh},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 docsh"},    % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, ?SHORT_DESC},
            {desc, ?DESC}
    ]),
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
    [ process_beam(State, B) || B <- BEAMs ].

-spec app_beam_files(rebar_app_info:t()) -> [file:filename()].
app_beam_files(App) ->
    EbinDir = rebar_app_info:ebin_dir(App),
    filelib:wildcard(filename:join([EbinDir, "*.beam"])).

-spec process_beam(rebar_state:t(), file:filename()) -> ok.
process_beam(_State, BEAM) ->
    try
        {ok, NewBEAM} = docsh_lib:process_beam(BEAM),
        ok = file:write_file(BEAM, NewBEAM)
    catch
        _:exdc_present -> ok
    end.
