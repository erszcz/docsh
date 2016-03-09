-module(docsh).

-export([main/1]).

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

main(Args) -> process_args(Args, opts()).

%%
%% Helpers
%%

opts() ->
    [ {"transform BEAMFile to NewBEAMFile", fun transform/1},
      {"diff BEAMFile1 BEAMFile2",          fun diff/1},
      {"help",                              fun usage/1} ].

transform(["transform", BEAMFile, "to", NewBEAMFile]) ->
    'try'(fun () -> {ok, NewBEAM} = docsh_lib:process_beam(BEAMFile),
                    ok = file:write_file(NewBEAMFile, NewBEAM) end).

diff(["diff", BEAM1, BEAM2]) ->
    'try'(fun () -> Diff = docsh_lib:beam_diff(BEAM1, BEAM2),
                    print("~p~n", [Diff]) end).

usage(_) ->
    usage(),
    erlang:halt(1).

process_args(Args, Opts) ->
    lists:foldl(fun process_arg/2, {next, Args}, Opts).

process_arg({_Desc, F}, done) -> done;
process_arg({_Desc, F}, {next, Args}) ->
    case catch erlang:apply(F, [Args]) of
        {'EXIT', {function_clause, _}} -> {next, Args};
        _ -> done
    end.

'try'(F) ->
    try F()
    catch
        _:R -> print(standard_error, "~s: ~s~n", [progname(), R]),
               print(standard_error, "~p~n", [erlang:get_stacktrace()]),
               erlang:halt(2)
    end.

usage() ->
    print(standard_error,
          "usage: ~s", [ [ [padding(I), progname(), " ", Desc, "\n"]
                           || {I, {Desc, _}} <- enum(opts()) ] ]).

padding(1) -> "";
padding(_) -> "       ".

enum(List) -> lists:zip(lists:seq(1, length(List)), List).

progname() ->
    filename:basename(hd(init:get_plain_arguments())).
