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
    CurrentApp = rebar_state:current_app(State),
    print("docsh do:~n", []),
    print("  beam files: ~p~n", [app_beam_files(CurrentApp)]),
    {ok, State}.

-spec app_beam_files(rebar_app_info:t()) -> [file:filename()].
app_beam_files(App) ->
    EbinDir = rebar_app_info:ebin_dir(App),
    filelib:wildcard(filename:join([EbinDir, "*.beam"])).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Helpers
%% ===================================================================

print(Format, Args) ->
    io:format(Format, Args).
