%% @doc
%% Welcome to docsh, the missing documentation viewer for the Erlang shell.
%% This project provides a number of helpers for accessing module and function
%% doc comments, function specs and exported types.
%%
%% To access this documentation you've quite likely typed:
%%
%% ```
%% h(docsh).
%% '''
%%
%% `h/{1,2,3}' is the most generic invocation of docsh.
%% `h/1' prints documentation for a given module (as shown above) or function:
%%
%% ```
%% h(fun lists:keyfind/3).
%% '''
%%
%% `h/{2,3}' are limited to functions.
%% `h/2' displays information about all functions of the given name from
%% a particular module, while `h/3' also takes the expected function arity:
%%
%% ```
%% h(proplists, get_value).
%% h(proplists, get_value, 3).
%% '''
%%
%% `s/{1,2,3}' is the helper to use if you're only interested in function specs,
%% not their full documentation:
%%
%% ```
%% s(fun proplists:get_value/3).
%% s(proplists, get_value).
%% s(proplists, get_value, 3).
%% '''
%%
%% `t/{2,3}' is the helper for printing types exported from modules:
%%
%% ```
%% t(gen_tcp, connect_option).
%% t(gen_udp, socket).
%% '''
%%
%% All the helpers described above are only available if you installed
%% the `user_default' extensions that ship with docsh.
%% See https://github.com/erszcz/docsh/README.md if unsure about
%% the installation steps or what `user_default' is.
%%
%% If you want to call docsh as a library, please refer to `docsh_erl'.
%% (TODO) As of now it's the only way to use docsh from Elixir's iex.
%% @end

-module(docsh).

%% Escript API
-export([main/1]).

%% Scripting API
-export([activated/1]).

-export_type([external/0,
              internal/0]).

-import(docsh_lib, [print/2, print/3]).

%% External documentation format.
%% Right now the only supported format is docsh_elixir_docs_v1 which
%% (TODO) aims to be compatible with Elixir at some point in the future.
-type external() :: any().

%% Internal documentation format.
%% All `docsh_reader` modules convert to this format from their input.
%% All `docsh_writer` modules convert from this format to desired output.
-type internal() :: [{atom(), any()}].

%%
%% Escript API
%%

-spec main([string()]) -> ok.
main(Args) ->
    process_args(Args, commands()).

%%
%% Scripting API
%%

-spec activated(path | user_default) -> ok.
activated(path) ->
    print("Enabled docsh from: ~s\n", [code:lib_dir(?MODULE)]);
activated(user_default) ->
    print("Call h(docsh) for interactive help.\n\n", []).

%%
%% Helpers
%%

commands() ->
    [ {"transform BEAMFile to NewBEAMFile", fun transform/1},
      {"diff BEAMFile1 BEAMFile2",          fun diff/1},
      {"help",                              fun usage/1} ].

transform(["transform", BEAMFile, "to", NewBEAMFile]) ->
    'try'(fun () -> {ok, NewBEAM, Warnings} = docsh_lib:process_beam(BEAMFile),
                    [ print("~s", [docsh_lib:format_error({W, BEAMFile})])
                      || W <- Warnings ],
                    ok = file:write_file(NewBEAMFile, NewBEAM) end).

diff(["diff", BEAM1, BEAM2]) ->
    'try'(fun () -> Diff = docsh_lib:beam_diff(BEAM1, BEAM2),
                    print("~p~n", [Diff]) end).

usage(_) ->
    usage(),
    erlang:halt(1).

process_args(Args, Opts) ->
    lists:foldl(fun process_arg/2, {next, Args}, Opts),
    ok.

process_arg({_Desc, _}, done) -> done;
process_arg({_Desc, F}, {next, Args}) ->
    case catch erlang:apply(F, [Args]) of
        {'EXIT', {function_clause, _}} -> {next, Args};
        _ -> done
    end.

'try'(F) ->
    try F()
    catch
        _:R -> print(standard_error, "~s: ~s~n~s~n",
                     [progname(), docsh_lib:format_error(R),
                      erlang:get_stacktrace()]),
               erlang:halt(2)
    end.

usage() ->
    print(standard_error,
          "usage: ~s", [ [ [padding(I), progname(), " ", Desc, "\n"]
                           || {I, {Desc, _}} <- enum(commands()) ] ]).

padding(1) -> "";
padding(_) -> "       ".

enum(List) -> lists:zip(lists:seq(1, length(List)), List).

progname() ->
    filename:basename(hd(init:get_plain_arguments())).
