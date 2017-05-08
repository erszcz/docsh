#!/usr/bin/env bash

HOME_ERLANG="$HOME/.erlang"
HOME_ERLANG_D="$HOME/.erlang.d"
HOME_USER_DEFAULT="$HOME_ERLANG_D/user_default.erl"

pushd $(dirname $0) > /dev/null
DOCSH_BASE=$PWD
popd > /dev/null

read -r -d '' INTRO <<EOF
Installing docsh

This install script will make docsh globally available in your
user environment.
It will install the following files:

  $HOME_ERLANG
  $HOME_USER_DEFAULT

To know more about these files please refer to:

  man erl - sections about 'The .erlang startup file'
            and 'user_default and shell_default'
  man shell_default - parts about user_default
EOF

read -r -d '' HOME_ERLANG_CONTENT <<EOF
proplists:is_defined(noshell, init:get_arguments()) == false andalso begin
    DocshBase = "$DOCSH_BASE",
    code:add_path(DocshBase ++ "/_build/default/lib/docsh/ebin"),
    io:format("Enabled docsh from: ~s\n", [DocshBase])
end.
code:load_abs(os:getenv("HOME") ++ "/.erlang.d/user_default").
EOF

read -r -d '' HOME_USER_DEFAULT_CONTENT <<EOF
-module(user_default).
-include("docsh_user_default.hrl").
EOF

echo
echo "$INTRO"
echo
read -p "Do you want to proceed? [y/N] " -n 1 -r
echo
echo

if [ ! x"$REPLY" = xy ]; then
    echo Nothing to do
    exit 0
fi

cd $DOCSH_BASE
./rebar3 compile
cd - > /dev/null
echo

PROCEED=yes

if [ -f "$HOME_ERLANG" ]; then
    echo "$HOME_ERLANG exists"
    PROCEED=no
fi

if [ -f "$HOME_USER_DEFAULT" ]; then
    echo "$HOME_USER_DEFAULT exists"
    PROCEED=no
fi

if [ x"$PROCEED" = xyes ]; then

    echo "$HOME_ERLANG_CONTENT" > "$HOME_ERLANG"

    mkdir -p "$HOME_ERLANG_D"
    cd $HOME_ERLANG_D
    echo "$HOME_USER_DEFAULT_CONTENT" > $HOME_USER_DEFAULT
    erlc -I $DOCSH_BASE/include $HOME_USER_DEFAULT > /dev/null
    cd - > /dev/null

    echo
    echo "Ok, it's done!"

else

    echo
    echo "Oops! It seems you have already customised your Erlang environment."
    echo "I'm not going to overwrite any of your settings,"
    echo "but here are the contents of the files that would be installed."
    echo
    echo "$HOME_ERLANG":
    echo
    echo "$HOME_ERLANG_CONTENT"
    echo
    echo "$HOME_USER_DEFAULT":
    echo
    echo "$HOME_USER_DEFAULT_CONTENT"
    echo
    echo "Don't forget to recompile $HOME_USER_DEFAULT if you change it!"
    echo

    exit 1

fi
