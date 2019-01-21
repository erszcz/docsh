%% This script enables docsh shell extensions in Rebar3 shell.
%% Make sure to have the following in rebar.config of your project:
%%
%%  {plugins,
%%   [
%%    {rebar3_docsh, "0.7.1", {pkg, docsh}}
%%   ]}.
%%  {shell, [{script_file, "_build/default/plugins/rebar3_docsh/script/docsh_rebar3_shell.escript"}]}.
%%
%% The reason why this script is necessary is that Rebar3 shell is run as an escript,
%% therefore due to -noshell option doesn't load user_default as erl does.
%% This means that even if you would have installed docsh system-wide,
%% shell extensions wouldn't be available in Rebar3 shell.
%% This script, when configured in rebar.config, dynamically loads user_default with docsh shell extensions.

main(_) ->
    docsh:load_shell_extensions().
