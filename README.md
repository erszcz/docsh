# Docs in the Shell

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in civilized languages like Python, Ruby or Elixir?

```
> recon:h().
Recon, as a module, provides access to the high-level functionality
contained in the Recon application.
...
> recon:h(get_state, 1).
Shorthand call to recon:get_state(PidTerm, 5000)
ok
> recon:h(get_state, 2).
Fetch the internal state of an OTP process.
  Calls sys:get_state/2 directly in R16B01+, and fetches
  it dynamically on older versions of OTP.
ok
>
```

You're in the right place.
`docsh` makes online (as in _when connected to a live system_,
not _in the internets_) access to documentation possible in Erlang.

## Try it

Just copy and paste:

```
git clone https://github.com/lavrin/docsh.git
cd docsh
./rebar3 ct
./rebar3 as test shell
recon:h().
```

## Use

Rebar3:

```erlang
{docsh, {git, "https://github.com/lavrin/docsh.git", {branch, master}}}
```

Rebar2, if you still have to (sorry, no semantic versioning yet):

```erlang
{docsh, ".*", {git, "https://github.com/lavrin/docsh.git", {branch, master}}}
```

Include the public header file in your module exposing a shell-usable API
with embedded documentation:

```erlang
-include_lib("docsh/include/pt_docsh.hrl").
```

The header contains a `parse_transform` directive which will embed the
documentation straight into the generated `.beam` file,
therefore making it accessible wherever you ship your code.
No extra build steps, no separate doc package - when you deploy your code,
you automagically deploy your docs.


## ToDo

In no particular order:

- [ ] Fix doc formatting (strip / add whitespace as needed?).

- [ ] Make whole [Recon](https://github.com/ferd/recon) compile with docsh.

- [ ] Extract specs from the AST.

- [ ] Figure out how to embed the docs into the "ExDc" chunk like Elixir does.
      It's most likely not possible from within a parse transformation.
      What would be a sensible approach?
      A Rebar3 plugin embedding the extra chunk after actual compilation?

- [ ] Add support for type spec extraction from the AST.

- [ ] `user_default` extensions:

    * [ ] to allow for functional `h(Mod)`, `h(Mod, Fun, Arity)`
          style calls

    * [ ] to enable reading Elixir embedded documentation;
          the same doc format is used intentionally,
          though for now docsh doesn't store docs in the same place as Elixir does


## ?!

Yes, I've seen _Ghost in the Shell_ ;)
