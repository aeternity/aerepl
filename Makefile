# Needed by rebar_localdep
export LOCALDEP_DIR = $(shell pwd)

# Node apps use it for versioning
export AETERNITY_VERSION $(cat node/VERSION)

.PHONY: all
all:  node aerepl

.PHONY: node
node:
	git submodule init
	git submodule update

.PHONY: aerepl
aerepl:
	./rebar3 as prod release
	chmod +x aerepl

.PHONY: docker
docker:
	@docker build -t aeternity/aerepl:local .

.PHONY: podman
podman:
	@podman build -t aeternity/aerepl:local .

.PHONY: test
test:
	./rebar3 eunit --module=aere_tests

.PHONY: clean
clean:
	./rebar3 clean

.PHONY: nuke
nuke:
	rm -rf _build
	rm -f rebar.lock
	cd node; ./rebar3 clean; rm -rf _build
