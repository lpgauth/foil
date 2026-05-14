REBAR3=$(shell which rebar3)
ifeq ($(REBAR3),)
REBAR3=./bin/rebar3
endif

all: compile

bench:
	@echo "Running bin/bench.sh..."
	@$(REBAR3) as test compile
	@./bin/bench.sh

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

dialyzer:
	@echo "Running rebar3 dialyzer..."
	@$(REBAR3) dialyzer

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv, cover -v

test: xref eunit dialyzer

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

.PHONY: bench clean compile dialyzer eunit test xref
