.PHONY: all aerepl test clean nuke

all: aerepl

aerepl:
	git submodule init
	git submodule update
	cd node && git checkout v6.4.0 && make dev1-build
	ln -sfn node/data .
	./rebar3 as prod release
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
