%% @doc Top-level module doc.
%% @end
-module(example).
-export([f/0]).

-include_lib("docsh/include/pt_docsh.hrl").

%% @doc Doc for f/0.
%% @end
-spec f() -> ok.
f() ->
    ok.
