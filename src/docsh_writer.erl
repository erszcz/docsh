-module(docsh_writer).

-callback from_internal(docsh_internal:t()) -> any().

-include_lib("docsh/include/docsh_exdoc.hrl").
