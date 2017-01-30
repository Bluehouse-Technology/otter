.PHONY: all compile clean ct test_deps

all: compile

compile:
	@rebar3 compile

shell:
	@rebar3 shell

compile_thrift:
	@thrift -gen erl priv/zipkinCore.thrift
	@thrift -gen erl priv/zipkinDependencies.thrift
	@thrift -gen erl priv/scribe.thrift
	@mv gen-erl/*.?rl src/
	@rm -rf gen-erl

ct: compile test_deps
	@echo "Running common tests..."
	-@ct_run -noshell -pa ebin \
		-pa deps/*/ebin \
		-pa test/deps/*/ebin \
		-name test \
		-logdir ./logs \
		-env TEST_DIR ./test \
		-spec ./test/test_specs.spec \
		-dir test >> ./logs/raw.log 2>&1
	@grep -h "TEST COMPLETE" logs/raw.log | tail -1

test_deps:
	@echo "Checking dependencies for test..."
	@cd test && rebar get-deps && rebar compile && cd ..

clean:
	@rebar clean
	@cd test && rebar clean && cd ..

distclean: clean
	@rm -rf ./deps/ && rm -rf ./test/deps/
