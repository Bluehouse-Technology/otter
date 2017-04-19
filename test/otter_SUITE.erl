-module(otter_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, ptest/1, ftest/1, handle_span/1]).

all() ->
    [ftest, ptest].

ptest(_Config) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(otter),
    application:set_env(otter, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    application:set_env(otter, server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]),
    otter_span_pdict_api:start("test_span"),
    ok = otter_span_pdict_api:log("started"),
    ok = otter_span_pdict_api:log("α =:= ω"),
    ok = otter_span_pdict_api:tag("result", "ok"),
    ok = otter_span_pdict_api:finish(),
    timer:sleep(200),
    [_] = ets:tab2list(test_span_collector).

ftest(_Config) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(otter),
    application:set_env(otter, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    application:set_env(otter, server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]),
    Span_id = otter:start("test_span"),
    ok = otter:log(Span_id, "started"),
    ok = otter:tag(Span_id, "result", "ok"),
    ok = otter:log(Span_id, "α =:= ω"),
    ok = otter:log(Span_id, 123456),
    ok = otter:log(Span_id, 'this is a atom'),
    ok = otter:log(Span_id, io_lib:format("int: ~w, float: ~f, hex: ~.16B, Span: ~p",
					  [1, 1.0, 1, Span_id])),
    ok = otter:log(Span_id, Span_id),
    ok = otter:log(Span_id, fun() -> "result of function" end),
    ok = otter:finish(Span_id),
    timer:sleep(200),
    [_] = ets:tab2list(test_span_collector).

handle_span(Span)  ->
    ets:insert(test_span_collector, Span).
