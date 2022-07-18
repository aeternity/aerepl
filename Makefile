.PHONY: all node aerepl test clean nuke nuke-all

all:  aerepl

node:
	git submodule init
	git submodule update
	cd node && git checkout v6.4.0 && make dev1-build

aerepl:
	./rebar3 as prod release
	chmod +x aerepl

test:
	./rebar3 eunit --module=aere_tests

clean:
	./rebar3 clean

nuke:
	rm -rf _build
	rm -f rebar.lock

nuke-all: nuke
	rm -rf node
