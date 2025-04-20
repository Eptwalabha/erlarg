#!/usr/bin/make

.PHONY : all deps compile dialyzer lint test cover clean
all: deps compile dialyzer linter cover
deps:
	rebar3 get-deps
compile:
	rebar3 compile
dialyzer:
	rebar3 dialyzer
lint:
	elvis rock --config elvis.config
linter:
	rebar3 lint
test:
	rebar3 eunit
cover:
	rebar3 eunit --cover
	rebar3 cover --verbose -m 100
clean:
	rebar3 clean
