.PHONY: all compile clean ct test_deps

REBAR=rebar3

all: compile

compile:
	@${REBAR} compile

shell:
	@${REBAR} shell

clean:
	@${REBAR} clean

distclean: clean
	@rm -rf ./_build/ && rm -rf ./test/deps/
