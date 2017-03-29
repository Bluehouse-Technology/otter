.PHONY: all compile clean ct upgrade shell

all: compile

compile:
	@rebar3 compile

shell:
	@rebar3 shell

ct: 
	@rebar3 ct --sys_config test/test_httpc.config
	@rebar3 ct --sys_config test/test_ibrowse.config

clean:
	@rebar3 clean

upgrade:
	@rebar3 upgrade
