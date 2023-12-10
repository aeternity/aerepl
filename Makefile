.PHONY: all node aerepl docker podman test clean nuke nuke-all

CXXFLAGS = -Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move

all:  node aerepl

node:
	git submodule init
	git submodule update
	cd node && make dev1-build

aerepl:
	./rebar3 as prod release
	chmod +x aerepl

docker:
	@docker build -t aeternity/aerepl:local .

podman:
	@podman build -t aeternity/aerepl:local .

test:
	./rebar3 eunit --module=aere_tests

clean:
	./rebar3 clean

nuke:
	rm -rf _build
	rm -f rebar.lock
	cd node; ./rebar3 clean; rm -rf _build
