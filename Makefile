.PHONY: elvis test dialyzer start

TAG ?= $(shell git log --format=%h -n 1)

elvis:
	elvis --config elvis.config

dialyzer:
	rebar3 dialyzer

start:
	rebar3 auto --apps egoc --config ./config/sys.config

test: elvis dialyzer
	rebar3 ct -v --config ./config/test.config --dir test 
