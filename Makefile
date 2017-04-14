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

test: ct

clean:
	@${REBAR} clean

upgrade:
	@${REBAR} upgrade

distclean: clean
	@rm -rf ./_build/ && rm -rf rebar.lock

rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

dialyzer:
	@${REBAR} dialyzer

docs:
	@${REBAR} edoc

xref:
	${REBAR} xref
