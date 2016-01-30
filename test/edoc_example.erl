%% @doc Top-level module doc.
%% @end
-module(edoc_example).
-export([f/0]).

-include_lib("docsh/include/pt_docsh.hrl").

-type r() :: ok.

%% @doc Doc for f/0.
%% @end
-spec f() -> r().
f() ->
    ok.
