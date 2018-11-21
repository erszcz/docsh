-module(ct_helper).
-compile([export_all, nowarn_export_all]).

%% @doc Like ct:get_config/1, but calls error/1 if Key is not found / undefined.
%% This guarantees to fail fast if required config options are missing from
%% the config file, saving trouble with `undefined` value propagation.
%% Use alongside with the CT `require` mechanism.
%% See s2s_SUITE for an example.
-spec get_config(atom()) -> any().
get_config(Key) ->
    Val = ct:get_config(Key),
    Val == undefined andalso erlang:error({undefined, Key}),
    Val.

test_specific_init(Module, CaseName, Config) ->
    case erlang:function_exported(Module, CaseName, 2) of
        true -> Module:CaseName(init, Config);
        false -> Config
    end.

test_specific_end(Module, CaseName, Config) ->
    case erlang:function_exported(Module, CaseName, 2) of
        true -> Module:CaseName(end_, Config);
        false -> Config
    end.
