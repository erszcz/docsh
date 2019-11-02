-module(docsh_reader).

-export_type([t/0]).

-type t() :: module().

%% Return reader module name if reader is available.
%% This might be based on support library availability in a constrained release
%% or information about the passed in beam file.
-callback available(docsh_beam:t()) -> R when
      R :: [docsh_reader:t()].

%% Read information about the beam file and return in docsh internal format.
-callback to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh_internal:t()}
         | {error, any(), [erlang:stack_item()]}.

