-ifndef(DOCSH_EXDOC_H).
-define(DOCSH_EXDOC_H, true).

-export(['__info__'/1]).

-spec '__info__'(compile) -> [{src, binary()}].
'__info__'(compile) ->
    case lists:keyfind(source, 1, erlang:get_module_info(?MODULE, compile)) of
        false -> erlang:error(no_compile_source_found);
        {source, Source} -> [{src, list_to_binary(Source)}]
    end.

-endif. %% DOCSH_EXDOC_H
