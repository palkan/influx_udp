REBAR=./rebar

all: clean deps compile

deps: get-deps update-deps

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

console:
	erl -pa deps/*/ebin -pa deps/*/include -pa ebin -s lager -s influx_udp

test: eunit ct

eunit:
	$(REBAR) eunit skip_deps=true

ct:
	$(REBAR) ct skip_deps=true
