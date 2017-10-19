## It works!

### base16

```erlang
1> h(base16).

# Module base16

## Description

Encoding and decoding of Base16-encoded binaries

## Types

ok
2> h(base16, encode).

base16:encode/1

-spec encode(binary()) -> <<_:_*16>>.

(description missing)


ok
3> s(base16, encode).

base16:encode/1

-spec encode(binary()) -> <<_:_*16>>.

ok
4> s(base16, decode).

base16:decode/1

-spec decode(<<_:_*16>>) -> binary().

ok
```

### bbmustache

```erlang
8> h(bbmustache).

# Module bbmustache

## Description

Binary pattern match Based Mustach template engine for Erlang/OTP.

Please refer to [the man page](http://mustache.github.io/mustache.5.html) and [the spec](https://github.com/mustache/spec) of mustache as the need arises.

Please see [this](../benchmarks/README.md) for a list of features that bbmustache supports.

## Types

-type key() :: binary().

-type source() :: binary().

-type tag() :: {n, [key()]} | {'&', [key()]} |
               {'#', [key()], [tag()], source()} | {'^', [key()], [tag()]} |
               {'>', key(), Indent :: source()} | binary().

-type state() :: #state{}.

-type data_key() :: atom() | binary() | string().

-type data_value() :: data() | iodata() | number() | atom() |
                      fun((data(), function()) -> iodata()).

-type assoc_data() :: [{atom(), data_value()}] | [{binary(), data_value()}] |
                      [{string(), data_value()}].

-type option() :: {key_type, atom | binary | string}.

-type data() :: assoc_data().

-type endtag() :: {endtag,
                   {state(), [key()], LastTagSize :: non_neg_integer(),
                    Rest :: binary(), Result :: [tag()]}}.

ok
9> h(fun bbmustache:compile/3).

bbmustache:compile/3

-spec compile(template(), data(), [option()]) -> binary().

Embed the data in the template.

  1> Template = bbmustache:parse_binary(<<"{{name}}">>).
  2> bbmustache:compile(Template, #{"name" => "Alice"}).
  <<"Alice">>

Data support assoc list or maps (OTP17 or later).
All key in assoc list or maps MUST be same type.

ok
```

## It even works from IEx

...though there's no handy interface yet.

```elixir
iex(1)> :docsh_erl.h(:lists, :keyfind, :any)

lists:keyfind/3

-spec keyfind(Key, N, TupleList) -> Tuple | false when Key :: term(),
                                                       N :: pos_integer(),
                                                       TupleList :: [Tuple],
                                                       Tuple :: tuple().

:ok
```
