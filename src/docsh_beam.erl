-module(docsh_beam).

-export([from_beam_file/1,
         from_loaded_module/1,

         name/1,
         abstract_code/1,
         beam_file/1,
         docs/1,
         source_file/1, source_file/2,
         attribute/2]).

-export_type([t/0]).

-record(docsh_beam, {name, beam_file, source_file}).

-opaque t() :: #docsh_beam{name :: module(),
                           %% `beam_file` has to be defined and is a file name.
                           %% TODO: Support dynamically compiled or network-loaded modules later.
                           beam_file :: file:filename(),
                           %% `source_file` may or may not be available.
                           source_file :: file:filename() | false}.

-type debug_info() :: [erl_parse:abstract_form() |
                       %% {eof, 123} is not an abstract_form()
                       erl_parse:form_info()].

%%
%% API
%%

-spec from_loaded_module(module()) -> {ok, t()} | {error, any()}.
from_loaded_module(Mod) ->
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

-spec name(t()) -> module().
name(B) -> B#docsh_beam.name.

-spec abstract_code(t()) -> debug_info() | false.
abstract_code(B) ->
    bind(docsh_lib:get_abstract_code(beam_file(B))).

-spec beam_file(t()) -> file:filename().
beam_file(B) -> B#docsh_beam.beam_file.

-spec docs(t()) -> docsh_format:t().
docs(#docsh_beam{} = B) ->
    case beam_lib:chunks(beam_file(B), ["Docs"]) of
        {ok, {_Mod, [{"Docs", BDocs}]}} ->
            erlang:binary_to_term(BDocs);
        {error, _, {missing_chunk, _, _}} ->
            %% TODO: should this module throw or return errors by value?
            erlang:error({no_docs, <<"no Docs chunk">>})
    end.

-spec source_file(t()) -> file:filename() | false.
source_file(B) -> B#docsh_beam.source_file.

-spec source_file(t(), file:filename()) -> t().
source_file(B, NewFile) -> B#docsh_beam{source_file = NewFile}.

-spec attribute(t(), atom()) -> term().
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
