-module(docsh).

-export_type([internal/0]).

%% Internal documentation format.
%% All `docsh_from` modules convert to this format from their input.
%% All `docsh_to` modules convert from this format to desired output.
-type internal() :: [{atom(), any()}].
