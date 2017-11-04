# Manual docsh setup

If you have already customised your Erlang/Elixir environment,
you might not want to rely on `install.sh` provided by docsh
or [kerl][kerl]'s `install-docsh` command, but still want to use docsh.
Below I'll show two different ways to achieve that:
the first using just your shell environment variable configuration,
and the second by the most basic scripting in `$HOME/.erlang`.
Treat these steps more as examples on how to integrate with your preferred
setup (for example, when you prefer [asdf][asdf] to kerl),
rather than the suggested way to use docsh - the most straightforward way
is to use `install.sh` or `kerl install-docsh`.

[kerl]: https://github.com/kerl/kerl
[asdf]: https://github.com/asdf-vm/asdf

## Enabling docsh via shell environment variables

First, docsh has to be in the Erlang code path.
I'm assuming here we've already cloned and built the project.
We have to export `ERL_LIBS` in the shell's `.profile`,
`.bash_profile`/`.bashrc`, `.zsh_profile`, etc.
In my case the `.bashrc` line would be:

```sh
export ERL_LIBS=/home/erszcz/work/erszcz/docsh/_build/default/lib
```

In order to add new helpers to the Erlang shell docsh relies on the
[`user_default` mechanism](http://erlang.org/doc/man/shell_default.html).
An example `user_default.erl` is bundled with docsh:

```erlang
%% file: docsh/templates/user_default.erl
-module(user_default).
-include("docsh_user_default.hrl").
```

As you can see, the file is trivial, as the helpers are actually defined
in `docsh_user_default.hrl`:

```erlang
%% file: docsh/include/docsh_user_default.hrl
-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/1, t/2, t/3]).

h(ModOrFun) -> docsh_erl:h(ModOrFun).
h(M, F)     -> docsh_erl:h(M, F).
h(M, F, A)  -> docsh_erl:h(M, F, A).

s(Fun)      -> docsh_erl:s(Fun).
s(M, F)     -> docsh_erl:s(M, F).
s(M, F, A)  -> docsh_erl:s(M, F, A).

t(M)        -> docsh_erl:t(M).
t(M, T)     -> docsh_erl:t(M, T).
t(M, T, A)  -> docsh_erl:t(M, T, A).
```

This makes it easy to include the header file in your own, possibly
heavily customised, `user_default.erl` file with minimum fuss.

Ok, let's make sure we have a compiled `user_default.beam` ready for
loading into the VM:

```sh
cd ~/work/erszcz/docsh
erlc -I include/ templates/user_default.erl
```

Now we need to make sure a starting Erlang shell is aware of the file
produced by the above command.
Put the following in your `$HOME/.erlang` file (please mind the missing
`.beam` extension):

```erlang
code:load_abs("/home/erszcz/work/erszcz/docsh/user_default").
```

That's it, we're all set. Let's check if the new settings work:

```
23:15:23 erszcz @ x2 : ~
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
1> h(docsh).

# docsh

Welcome to docsh, the missing documentation viewer for the Erlang shell.
...
```

## Enabling docsh via `$HOME/.erlang` scripting

This method differs from the previous one only in the way we
extend the Erlang code path.
Instead of relying on `ERL_LIBS`, we hardcode the path in `$HOME/.erlang`.
Therefore, we first have to compile `user_default.beam` (see the previous
section for an explanation of what this file is needed for):

```sh
cd ~/work/erszcz/docsh
erlc -I include/ templates/user_default.erl
```

Once it's done, we make sure `$HOME/.erlang` contains the following:

```erlang
code:add_path("/home/erszcz/work/erszcz/docsh/_build/default/lib/docsh/ebin").
code:load_abs("/home/erszcz/work/erszcz/docsh/user_default").
```

Let's check if the new settings work:

```
23:15:23 erszcz @ x2 : ~
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
1> h(docsh).

# docsh

Welcome to docsh, the missing documentation viewer for the Erlang shell.
...
```
