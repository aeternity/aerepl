# Compatibility reasons. rocksdb may not build without these
CXXFLAGS="-Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move"
.PHONY: test


all: aerepl

aerepl:
	git submodule init
	git submodule update
	ln -sfn node/apps .
	ln -sfn node/data .
	CXXFLAGS="-Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move" ./rebar3 as prod release
	chmod +x aerepl
	ln -sfn _build/prod/rel/aerepl/bin/aerepl aerepl_server

test:
	./rebar3 eunit --module=aere_tests

clean:
	./rebar3 clean

nuke:
	rm apps -rf
	rm node -rf
	rm _build -rf
	rm rebar.lock -f
