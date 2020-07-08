.PHONY: elvis test dialyzer start

TAG ?= $(shell git log --format=%h -n 1)
APP_NAME := pipeflow

elvis:
	elvis --config elvis.config

dialyzer:
	rebar3 dialyzer

start:
	rebar3 auto --apps egoc --config ./config/sys.config

test: elvis dialyzer
	rebar3 ct -v --config ./config/test.config --dir test 
