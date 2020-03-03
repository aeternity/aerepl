# Compatibility reasons. rocksdb may not build without these
CXXFLAGS="-Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move"

all: aerepl

aerepl:
	git submodule init
	git submodule update
	ln -s node/apps apps
	CXXFLAGS="-Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move" ./rebar3 compile
	echo "\e[1mTo launch run ./rebar3 shell and type aerepl:start() in the prompt.\e[0m"

clean:
	./rebar3 clean

nuke:
	rm apps -rf
	rm node -rf
	rm _build -rf
