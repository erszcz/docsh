-module(docsh_beam).

-export([from_beam_file/1,
         from_loadable_module/1,

         name/1,
         abst/1,
         beam_file/1,
         source_file/1,
         attribute/2]).

-export_type([t/0]).

-record(docsh_beam, {name, beam_file, source_file}).

-opaque t() :: #docsh_beam{name :: module(),
                           %% `beam_file` has to be defined and is a file name.
                           %% TODO: Support dynamically compiled or network-loaded modules later.
                           beam_file :: file:filename(),
                           %% `source_file` may or may not be available.
                           source_file :: file:filename() | false}.

%%
%% API
%%

-spec from_loadable_module(module()) -> {ok, t()} | {error, any()}.
from_loadable_module(Mod) ->
    case code:which(Mod) of
        E when E =:= non_existing;
               E =:= cover_compiled;
               E =:= preloaded ->
            {error, {no_beam_file, E}};
        BEAMFile ->
            {ok, #docsh_beam{name = Mod,
                             beam_file = BEAMFile,
                             source_file = bind(docsh_lib:get_source_file(BEAMFile))}}
    end.

-spec from_beam_file(file:filename()) -> {ok, t()} | {error, any()}.
from_beam_file(BEAMFile) ->
    {ok, #docsh_beam{name = beam_name(BEAMFile),
                     beam_file = BEAMFile,
                     source_file = bind(docsh_lib:get_source_file(BEAMFile))}}.

name(B) -> B#docsh_beam.name.

abst(B) ->
    debug_info(beam_file(B)).

beam_file(B) -> B#docsh_beam.beam_file.

source_file(B) -> B#docsh_beam.source_file.

attribute(B, Name) ->
    docsh_lib:get(Name, (B#docsh_beam.name):module_info(attributes)).

%%
%% Helpers
%%

bind({ok, V}) -> V;
bind(false)   -> false.

beam_name(BEAMFile) ->
    {ok, N, _} = beam_lib:all_chunks(BEAMFile),
    N.

debug_info(BEAMFile) ->
    case bind(docsh_lib:get_debug_info(BEAMFile)) of
        false -> false;
        BAbst when is_binary(BAbst) ->
            {raw_abstract_v1, Abst} = binary_to_term(BAbst),
            Abst
    end.
