-module(docsh_elixir_docs_v1).
-export([module/1,
	 function/1]).

module(_) ->
    {module}.

%% Example:
%% {{cast,2},
%%  274,def,
%%  [{agent,[],nil},{'fun',[],nil}],
%%  <<"Performs a cast (fire and forget) operation on the agent state...">>}.
function(_) ->
    {function}.
