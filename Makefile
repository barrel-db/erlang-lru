REBAR?=$(shell which rebar3)
DIALYZER?=dialyzer
PLT=.hackney.plt

all: build

doc:
	$(REBAR) edoc

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

test:
	$(REBAR) eunit

# development
#
dialyzer:
	$(REBAR) dialyzer


.PHONY: doc
