%% This script enables docsh shell extensions in Rebar3 shell.
%% Make sure to have the following in rebar.config of your project:
%%
%%  {plugins,
%%   [
%%    {rebar3_docsh, "*", {git, "https://github.com/erszcz/docsh", {ref, "ee59517"}}}
%%   ]}.
%%  {shell, [{script_file, "_build/default/plugins/rebar3_docsh/script/docsh_rebar3_shell.escript"}]}.
%%
%% The reason why this script is necessary is that Rebar3 shell is run as an escript,
%% therefore due to -noshell option doesn't load user_default as erl does.
%% This means that even if you would have installed docsh system-wide,
%% shell extensions wouldn't be available in Rebar3 shell.
%% This script, when configured in rebar.config, dynamically loads user_default with docsh shell extensions.

main(_) ->
    docsh_beam:from_loaded_module(docsh_user_default),
    {ok, B} = docsh_beam:from_loaded_module(docsh_user_default),
    Forms = docsh_beam:abstract_code(B),
    NewForms = lists:map(fun ({attribute,_,module,_}) -> {attribute,1,module,user_default};
                             (F)                      -> F end, Forms),
    {ok, Mod, ModBin} = compile:forms(NewForms),
    code:load_binary(Mod, "docsh_user_default.erl", ModBin).
