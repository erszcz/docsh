-module(docsh_reader).

-callback to_internal(file:filename()) -> docsh:internal().
