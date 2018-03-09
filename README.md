# Docs in the Shell

[![TravisCI Build Status](https://travis-ci.org/erszcz/docsh.svg?branch=master)](https://travis-ci.org/erszcz/docsh)

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in modern languages like Python, Ruby or Elixir?

[![docsh - light and dark background](https://raw.githubusercontent.com/erszcz/docsh/wip/doc/light-dark-bg.png)](https://github.com/erszcz/docsh/blob/wip/doc/light-dark-bg.png)

You're in the right place.
`docsh` makes online (as in _when connected to a live system_,
not _in the internets_) access to documentation possible in Erlang.


## Installation

```
git clone https://github.com/erszcz/docsh
cd docsh
./install.sh
```

The script will ask if you're sure you want to create some
Erlang configuration files:

```
$ ./install.sh

Installing docsh

This install script will make docsh globally available in your
user environment.
It will install the following files:

  /Users/erszcz/.erlang
  /Users/erszcz/.erlang.d/user_default.erl

To know more about these files please refer to:

  man erl - sections about 'The .erlang startup file'
            and 'user_default and shell_default'
  man shell_default - parts about user_default

Do you want to proceed? [y/N]
```

Even if you agree to install, but the target files exist,
it won't proceed - don't worry if you have customized your
Erlang environment already.

Once the installation is complete,
`erl` will greet you in a bit different way than you might be used to:

```erlang
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Enabled docsh from: /Users/erszcz/work/erszcz/docsh
Eshell V8.2  (abort with ^G)
1>
```

Now you can rely on your chosen package's documentation as the guide
to its use:

```erlang
1> h(proplists, get_value, 3).

proplists:get_value/3

-spec get_value(Key, List, Default) -> term() when Key :: term(),
                                                   List :: [term()],
                                                   Default :: term().

Returns the value of a simple key/value property in
List. If lookup(Key, List) would yield
{Key, Value}, this function returns the corresponding
Value, otherwise Default is returned.

ok
2> h(proplists).

# proplists

Support functions for property lists.

...
```

Well, your mileage may vary ;p
Check out [examples.md](examples.md) for how the docs are formatted
when they actually are provided for a project.
Better yet, just play with docsh with a project of your choice
and let me know of the experience!


## Usage

Let's see what docsh can give us for some OTP modules.
We call `h/2` to get the doc for `lists:keyfind` no matter the arity:

```
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
1> h(lists, keyfind).

lists:keyfind/3

-spec keyfind(Key, N, TupleList) -> Tuple | false when Key :: term(),
                                                       N :: pos_integer(),
                                                       TupleList :: [Tuple],
                                                       Tuple :: tuple().

Documentation is not available.


ok
2>
```

Let's try with Recon:

```
git clone https://github.com/ferd/recon
cd recon
./rebar compile
erl -pa ebin/
```

Once in the Erlang shell:

```erlang
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
> s(recon_trace, calls).

recon_trace:calls/2

-spec calls(tspec() | [tspec(), ...], max()) -> num_matches().


recon_trace:calls/3

-spec calls(tspec() | [tspec(), ...], max(), options()) -> num_matches().

ok
> h(recon_trace, calls, 2).

recon_trace:calls/2

-spec calls(tspec() | [tspec(), ...], max()) -> num_matches().

Equivalent to calls({Mod, Fun, Args}, Max, [])
See calls/3

ok
>
```

As you can see above, `s/2` gives us just the function specs.
Having read them, we want a more detailed description of `recon_trace:calls/2`,
so we ask for the doc and specify the arity with `h/3`.

Try it with your project!


## ?!

Yes, I've seen _Ghost in the Shell_ ;)


[edoc:module-tags]: http://erlang.org/doc/apps/edoc/chapter.html#Module_tags
[gh:recon-docsh]: https://github.com/erszcz/recon
