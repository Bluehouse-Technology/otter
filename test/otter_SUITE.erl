-module(otter_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("otter_lib/src/otter.hrl").

-export([
    all/0,
    ptest/1,
    ftest/1,
    mptest/1,
    idtest/1,
    ptest_prefilter/1,
    mptest_prefilter/1,
    idtest_prefilter/1,
    filter_one_out_of/1,
    handle_span/1
]).

all() ->
    [
        ftest, ptest, mptest, idtest,
        ptest_prefilter, mptest_prefilter, idtest_prefilter,
        filter_one_out_of
    ].


%% Tests

%% Basic tests for different APIs

ptest(_Config) ->
    setup(),
    otter_span_pdict_api:start("test_span"),
    otter_span_pdict_api:log("started"),
    otter_span_pdict_api:log("α =:= ω"),
    otter_span_pdict_api:tag("result", "ok"),
    otter_span_pdict_api:finish(),
    [
        #span{
            name = <<"test_span">>,
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"result">>, <<"ok">>}
            ]
        }
    ] = finish().

mptest(_Config) ->
    setup(),
    otter_span_mpdict_api:start("test_span1"),
    otter_span_mpdict_api:start("test_span2"),
    otter_span_mpdict_api:log("test_span1", "started"),
    otter_span_mpdict_api:log("test_span1", "α =:= ω"),
    otter_span_mpdict_api:tag("test_span1", "result", "ok"),
    otter_span_mpdict_api:finish("test_span1"),
    otter_span_mpdict_api:log("test_span2", "started"),
    otter_span_mpdict_api:log("test_span2", "α =:= ω"),
    otter_span_mpdict_api:tag("test_span2", "result", "ok"),
    otter_span_mpdict_api:finish("test_span2"),
    [_, _] = finish().


idtest(_Config) ->
    setup(),
    S1 = otter_span_id_api:start("test_span1"),
    S2 = otter_span_id_api:start("test_span2"),
    otter_span_id_api:log(S1, "started"),
    otter_span_id_api:log(S1, "α =:= ω"),
    otter_span_id_api:tag(S1, "result", "ok"),
    otter_span_id_api:finish(S1),
    otter_span_id_api:log(S2, "started"),
    otter_span_id_api:log(S2, "α =:= ω"),
    otter_span_id_api:tag(S2, "result", "ok"),
    otter_span_id_api:finish(S2),
    [_, _] = finish().


ftest(_Config) ->
    setup(),
    S1 = otter:start("test_span"),
    S2 = otter:log(S1, "started"),
    S3 = otter:tag(S2, "result", "ok"),
    S4 = otter:log(S3, "α =:= ω"),
    S5 = otter:log(S4, 123456),
    S6 = otter:log(S5, 'this is a atom'),
    S7 = otter:log(S6, io_lib:format("int: ~w, float: ~f, hex: ~.16B, Span: ~p",
                      [1, 1.0, 1, S6])),
    S8 = otter:log(S7, S7),
    S9 = otter:log(S8, fun() -> "result of function" end),
    otter:finish(S9),
    [_] = finish().


%% Prefilter test for different APIs
ptest_prefilter(_Config) ->
    setup(),
    otter_config:write(
        prefilter_rules,
        [
            {
                [{value, otter_span_name, "test_span"}],
                [discard]
            }
        ]
    ),
    otter_span_pdict_api:start_with_tags("test_span", [{"initial_tag", "true"}]),
    otter_span_pdict_api:log("started"),
    otter_span_pdict_api:log("α =:= ω"),
    otter_span_pdict_api:tag("result", "ok"),
    otter_span_pdict_api:finish(),
    [] = finish().

mptest_prefilter(_Config) ->
    setup(),
    otter_config:write(
        prefilter_rules,
        [
            {
                [{value, otter_span_name, "test_span1"}],
                [discard]
            }
        ]
    ),
    otter_span_mpdict_api:start_with_tags("test_span1", [{initial_tag, 1}]),
    otter_span_mpdict_api:start_with_tags("test_span2", [{initial_tag, 2}]),
    otter_span_mpdict_api:log("test_span1", "started"),
    otter_span_mpdict_api:log("test_span1", "α =:= ω"),
    otter_span_mpdict_api:tag("test_span1", "result", "ok"),
    otter_span_mpdict_api:finish("test_span1"),
    otter_span_mpdict_api:log("test_span2", "started"),
    otter_span_mpdict_api:log("test_span2", "α =:= ω"),
    otter_span_mpdict_api:tag("test_span2", "result", "ok"),
    otter_span_mpdict_api:finish("test_span2"),
    [
        #span{
            name = <<"test_span2">>,
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"initial_tag">>, <<"2">>},
                {<<"result">>, <<"ok">>}
            ]
        }
    ] = finish().

idtest_prefilter(_Config) ->
    setup(),
    otter_config:write(
        prefilter_rules,
        [
            {
                [{value, otter_span_name, "test_span1"}],
                [discard]
            }
        ]
    ),
    S1 = otter_span_id_api:start_with_tags("test_span1", [{initial_tag, 1}]),
    S2 = otter_span_id_api:start_with_tags("test_span2", [{initial_tag, 2}]),
    otter_span_id_api:log(S1, "started"),
    otter_span_id_api:log(S1, "α =:= ω"),
    otter_span_id_api:tag(S1, "result", "ok"),
    otter_span_id_api:finish(S1),
    otter_span_id_api:log(S2, "started"),
    otter_span_id_api:log(S2, "α =:= ω"),
    otter_span_id_api:tag(S2, "result", "ok"),
    otter_span_id_api:finish(S2),
    [
        #span{
            name = <<"test_span2">>,
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"initial_tag">>, <<"2">>},
                {<<"result">>, <<"ok">>}
            ]
        }
    ] = finish().


%% This might deserve a more proper test. Looking at the spread of some
%% manual tests I believe on a much larger sample it converges fairly close
%% to the desired selection. The +-20% here is for the sake of keeping this
%% test (probably/mostly) ok for now.
filter_one_out_of(_Config) ->
    L = length([
        1 || [ok] <- [
            otter_lib_filter:run(
                [{tag1, value1}],
                [{[{one_out_of, 1000}], [ok]}]
            ) || _ <- lists:seq(1, 100000)
        ]
    ]),
    true = if L < 120 andalso L > 80 -> true; true -> false end.

%% Server span handler
handle_span(Span)  ->
    ets:insert(test_span_collector, Span).

%% Helpers
setup() ->
    application:ensure_all_started(otter),
    application:ensure_all_started(otter_srv),
    otter_config:write(zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    otter_srv_config:write(server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]).


finish() ->
    timer:sleep(200),
    Result = ets:tab2list(test_span_collector),
    ets:delete(test_span_collector),
    Result.

