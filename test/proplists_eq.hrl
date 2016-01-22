-define(proplists_eq(Expected, Actual),
        (fun (__Expected, __Actual) ->
                 ?assertEqual(lists:sort(__Expected),
                              lists:sort(__Actual))
         end)(Expected, Actual)).
