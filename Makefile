activate: _build/default/lib/docsh
	echo "export ERL_LIBS=\"$(PWD)/_build/default/lib:$${ERL_LIBS}\"" > activate

_build/default/lib/docsh:
	./rebar3 compile
