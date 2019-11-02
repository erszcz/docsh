-module(docsh_user_default).

%% To avoid:
%% > include/docsh_user_default.hrl:1: attribute export after function definitions
%% We have to define and export the ExDoc compat manually.
%-include_lib("docsh/include/docsh_exdoc.hrl").

-export(['__info__'/1]).

-include("docsh_user_default.hrl").

-spec '__info__'(compile) -> [{src, binary()}].
'__info__'(compile) ->
    case lists:keyfind(source, 1, erlang:get_module_info(?MODULE, compile)) of
        false -> erlang:error(no_compile_source_found);
        {source, Source} -> [{src, list_to_binary(Source)}]
    end.
