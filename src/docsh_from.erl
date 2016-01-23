-module(docsh_from).

-callback to_internal(file:filename()) -> docsh:internal().
