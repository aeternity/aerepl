all: aerepl

aerepl:
	cp -r node/apps .
	echo "To launch run ./rebar3 shell and type aerepl:start() in the prompt."

clean:
	"./rebar3 clean"

nuke:
	"rm _build -rf"
