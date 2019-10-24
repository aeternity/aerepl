all: aerepl

aerepl:
	cp -r node/apps .
	@echo "\e[1mTo launch run ./rebar3 shell and type aerepl:start() in the prompt.\e[0m"

clean:
	"./rebar3 clean"

nuke:
	"rm _build -rf"
