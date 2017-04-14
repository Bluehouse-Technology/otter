-module(otter_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, ptest/1, ftest/1, handle_span/1]).

all() ->
    [ftest, ptest].

ptest(_Config) ->
    application:ensure_all_started(otter),
    application:set_env(otter, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    application:set_env(otter, server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]),
    otter_span_pdict_api:start("test_span"),
    otter_span_pdict_api:log("started"),
    otter_span_pdict_api:tag("result", "ok"),
    otter_span_pdict_api:finish(),
    timer:sleep(200),
    [_] = ets:tab2list(test_span_collector).

ftest(_Config) ->
    application:ensure_all_started(otter),
    application:set_env(otter, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    application:set_env(otter, server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]),
    S1 = otter:start("test_span"),
    S2 = otter:log(S1, "started"),
    S3 = otter:tag(S2, "result", "ok"),
    otter:finish(S3),
    timer:sleep(200),
    [_] = ets:tab2list(test_span_collector).

handle_span(Span)  ->
    ets:insert(test_span_collector, Span).
