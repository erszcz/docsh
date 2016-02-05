-module(docsh_prettypr).

-export([type/1,
         type_attr/1]).

-export([t_to_string/1, t_to_string/2]).

-spec type_attr(Attr) -> iodata() when
      Attr :: erl_parse:abstract_form().
type_attr({attribute, _Loc, type, Data}) ->
    {_TypeName, Form, _} = Data,
    type(Form).

-spec type(Type) -> iodata() when
      Type :: erl_parse:abstract_form().
type(Form) ->
    ExpTypes = sets:new(),
    Site = {type, {m, t, 0}},
    RecDict = dict:new(),
    CRecord = erl_types:t_from_form(Form, ExpTypes, Site, RecDict),
    t_to_string(CRecord).

-define(any,  any).
-define(none, none).
-define(unit, unit).
%% Generic constructor - elements can be many things depending on the tag.
-record(c, {tag,
            elements = [],
            qualifier}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.

-record(remote,  {mod:: module(), name :: atom(), args = [] :: [erl_type()]}).

-define(function_tag,   function).
-define(product_tag,    product).
-define(remote_tag,     remote).
-define(union_tag,      union).

-define(function(Domain, Range),   #c{tag=?function_tag, 
                                      elements=[Domain, Range]}).
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(remote(RemTypes),          #c{tag=?remote_tag, elements=RemTypes}).
-define(union(List), #c{tag=?union_tag, elements=[_,_,_,_,_,_,_,_,_,_,_]=List}).

%-spec t_to_string(erl_type()) -> string().

t_to_string(T) ->
    t_to_string(T, dict:new()).

%-spec t_to_string(erl_type(), type_table()) -> string().

t_to_string(?function(?product(ArgList), Range), RecDict) ->
    "fun((" ++ comma_sequence(ArgList, RecDict) ++ ") -> "
    ++ t_to_string(Range, RecDict) ++ ")";

%t_to_string(?identifier(Set), _RecDict) ->
%    case Set of
%        ?any -> "identifier()";
%        _ ->
%            string:join([flat_format("~w()", [T]) || T <- set_to_list(Set)], " | ")
%    end;

%t_to_string(?opaque(Set), RecDict) ->
%    string:join([opaque_type(Mod, Name, Args, S, RecDict) ||
%                 #opaque{mod = Mod, name = Name, struct = S, args = Args}
%                 <- set_to_list(Set)],
%                " | ");

%t_to_string(?nonempty_list(Contents, Termination), RecDict) ->
%    ContentString = t_to_string(Contents, RecDict),
%    case Termination of
%        ?nil ->
%            case Contents of
%                ?char -> "nonempty_string()";
%                _ -> "["++ContentString++",...]"
%            end;
%        ?any -> 
%            %% Just a safety check.
%            case Contents =:= ?any of
%                true -> ok;
%                false ->
%                    %% XXX. See comment below.
%                    %% erlang:error({illegal_list, ?nonempty_list(Contents, Termination)})
%                    ok
%            end,
%            "nonempty_maybe_improper_list()";
%        _ ->
%            case t_is_subtype(t_nil(), Termination) of
%                true ->
%                    "nonempty_maybe_improper_list("++ContentString++","
%                    ++t_to_string(Termination, RecDict)++")";
%                false ->
%                    "nonempty_improper_list("++ContentString++","
%                    ++t_to_string(Termination, RecDict)++")"
%            end
%    end;

%t_to_string(?list(Contents, Termination, ?unknown_qual), RecDict) ->
%    ContentString = t_to_string(Contents, RecDict),
%    case Termination of
%        ?nil ->
%            case Contents of
%                ?char -> "string()";
%                _ -> "["++ContentString++"]"
%            end;
%        ?any ->
%            %% Just a safety check.      
%            %% XXX. Types such as "maybe_improper_list(integer(), any())"
%            %% are OK, but cannot be printed!?
%            case Contents =:= ?any of
%                true -> ok;
%                false ->
%                    ok
%                    %% L = ?list(Contents, Termination, ?unknown_qual),
%                    %% erlang:error({illegal_list, L})
%            end,
%            "maybe_improper_list()";
%        _ -> 
%            case t_is_subtype(t_nil(), Termination) of
%                true ->
%                    "maybe_improper_list("++ContentString++","
%                    ++t_to_string(Termination, RecDict)++")";
%                false ->
%                    "improper_list("++ContentString++","
%                    ++t_to_string(Termination, RecDict)++")"
%            end
%    end;

%t_to_string(?product(List), RecDict) -> 
%    "<" ++ comma_sequence(List, RecDict) ++ ">";

t_to_string(?remote(Set), RecDict) ->
    string:join([case Args =:= [] of
                     true  -> flat_format("~w:~w()", [Mod, Name]);
                     false ->
                         ArgString = comma_sequence(Args, RecDict),
                         flat_format("~w:~w(~s)", [Mod, Name, ArgString])
                 end
                 || #remote{mod = Mod, name = Name, args = Args} <-
                    set_to_list(Set)],
                " | ");

%t_to_string(?map(Pairs), RecDict) ->
%    "#{" ++ map_pairs_to_string(Pairs,RecDict) ++ "}";

t_to_string(?tuple(Elements, _Arity, ?any), RecDict) ->   
    "{" ++ comma_sequence(Elements, RecDict) ++ "}";

%t_to_string(?tuple(Elements, Arity, Tag), RecDict) ->
%    [TagAtom] = atom_vals(Tag),
%    case lookup_record(TagAtom, Arity-1, RecDict) of
%        error -> "{" ++ comma_sequence(Elements, RecDict) ++ "}";
%        {ok, FieldNames} ->
%            record_to_string(TagAtom, Elements, FieldNames, RecDict)
%    end;

%t_to_string(?tuple_set(_) = T, RecDict) ->
%    union_sequence(t_tuple_subtypes(T), RecDict);

t_to_string(?union(Types), RecDict) ->
    union_sequence([T || T <- Types, T =/= ?none], RecDict);

t_to_string(Other, RecDict) ->
    erl_types:t_to_string(debug('t_to_string:pass', Other), RecDict).

comma_sequence(Types, RecDict) ->
    List = [case T =:= ?any of
                true -> "_";
                false -> t_to_string(T, RecDict)
            end || T <- Types],
    string:join(List, ",").

union_sequence(Types, RecDict) ->
    List = [t_to_string(T, RecDict) || T <- Types], 
    string:join(List, "\n| ").

set_to_list(Set) ->
    ordsets:to_list(Set).

flat_format(F, S) ->
  lists:flatten(io_lib:format(F, S)).

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.
