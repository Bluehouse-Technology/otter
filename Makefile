.PHONY: all compile clean ct upgrade shell distclean

REBAR=rebar3

all: compile

compile:
	@${REBAR} compile

shell:
	@${REBAR} shell

ct: 
	@${REBAR} ct --sys_config test/test_httpc.config
	@${REBAR} ct --sys_config test/test_ibrowse.config

clean:
	@${REBAR} clean

upgrade:
	@${REBAR} upgrade

distclean: clean
	@rm -rf ./_build/ && rm -rf rebar.lock
