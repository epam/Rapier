REBAR = ./rebar

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa ebin deps/*/ebin -s restservice )

.PHONY: all deps compile clean run
