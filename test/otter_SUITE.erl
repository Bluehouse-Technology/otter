-module(otter_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("otter_lib/include/otter.hrl").

-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0,
    ptest/1,
    ftest/1,
    mptest/1,
    idtest/1,
    test_start_with_tags/1,
    ptest_prefilter/1,
    mptest_prefilter/1,
    idtest_prefilter/1,
%%    filter_one_out_of/1,
    handle_span/1,
    custom_http_client_cb/2
]).


init_per_suite(Config) ->
    ets_start(),
    application:ensure_all_started(otter),
    application:ensure_all_started(ibrowse),
    %% skipping hackney from these tests as it requires 18+ OTP, it can
    %% be used of course but it'll reduce backward compatibility and travis
    %% tests otter with the other clients from R16.
    %% application:ensure_all_started(hackney),
    application:ensure_all_started(otter_srv),
    otter_config:write(zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    otter_srv_config:write(server_zipkin_callback, {?MODULE, handle_span}),
    Config.

end_per_suite(_) ->
    ets_stop().

all() ->
    [
        ftest, ptest, mptest, idtest,
        test_start_with_tags,
        ptest_prefilter, mptest_prefilter, idtest_prefilter
  %%      ,filter_one_out_of
    ].


%% Tests

%% Basic tests for different APIs

ptest(_Config) ->
    otter_span_pdict_api:start("test_span"),
    otter_span_pdict_api:log("started"),
    otter_span_pdict_api:log(<<206,177,32,61,58,61,32,207,137>>),
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
    otter_span_mpdict_api:start("test_span1"),
    otter_span_mpdict_api:start("test_span2"),
    otter_span_mpdict_api:log("test_span1", "started"),
    otter_span_mpdict_api:log("test_span1", <<206,177,32,61,58,61,32,207,137>>),
    otter_span_mpdict_api:tag("test_span1", "result", "ok"),
    otter_span_mpdict_api:finish("test_span1"),
    otter_span_mpdict_api:log("test_span2", "started"),
    otter_span_mpdict_api:log("test_span2", <<206,177,32,61,58,61,32,207,137>>),
    otter_span_mpdict_api:tag("test_span2", "result", "ok"),
    otter_span_mpdict_api:finish("test_span2"),
    [
        #span{
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"result">>, <<"ok">>}
            ]
        },
        #span{
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


idtest(_Config) ->
    S1 = otter_span_id_api:start("test_span1"),
    S2 = otter_span_id_api:start("test_span2"),
    otter_span_id_api:log(S1, "started"),
    otter_span_id_api:log(S1, <<206,177,32,61,58,61,32,207,137>>),
    otter_span_id_api:tag(S1, "result", "ok"),
    otter_span_id_api:finish(S1),
    timer:sleep(100),
    otter_span_id_api:log(S2, "started"),
    otter_span_id_api:log(S2, <<206,177,32,61,58,61,32,207,137>>),
    otter_span_id_api:tag(S2, "result", "ok"),
    otter_span_id_api:finish(S2),
    [
        #span{
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"result">>, <<"ok">>}
            ]
        },
        #span{
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


ftest(_Config) ->
    S1 = otter:start("test_span"),
    S2 = otter:log(S1, "started"),
    S3 = otter:tag(S2, "result", "ok"),
    S4 = otter:log(S3, <<206,177,32,61,58,61,32,207,137>>),
    S5 = otter:log(S4, 123456),
    S6 = otter:log(S5, 'this is a atom'),
    S7 = otter:log(S6, io_lib:format("int: ~w, float: ~f, hex: ~.16B, Span: ~p",
                      [1, 1.0, 1, S6])),
    S8 = otter:log(S7, S7),
    S9 = otter:log(S8, fun() -> "result of function" end),
    otter:finish(S9),
    [
        #span{
            name = <<"test_span">>,
            logs = [
                {_, <<"started">>},
                {_, <<206,177,32,61,58,61,32,207,137>>},
                {_, <<"123456">>},
                {_, <<"this is a atom">>},
                {_, <<"int: 1, float: 1.000000, hex: 1, Span: {span", _/bytes>>},
                {_, <<"{span", _/bytes>>},
                {_, <<"result of function">>}
            ],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"result">>, <<"ok">>}
            ]
        }
    ] = finish().

test_start_with_tags(_Config) ->
    otter:finish(
       otter:start_with_tags("test_span", [{"initial_tag", "true"}])),
    [
        #span{
            name = <<"test_span">>,
            logs = [],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"initial_tag">>, <<"true">>}
            ]
        }
    ] = finish(),

    otter:finish(
       otter:start_with_tags("test_span2", [{"initial_tag", "true"}], 123)),
    [
        #span{
            name = <<"test_span2">>,
            trace_id = 123,
            logs = [],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"initial_tag">>, <<"true">>}
            ]
        }
    ] = finish(),

    otter:finish(
       otter:start_with_tags("test_span3", [{"initial_tag", "true"}], 123, 321)),
    [
        #span{
            name = <<"test_span3">>,
            trace_id = 123,
            parent_id = 321,
            logs = [],
            tags = [
                {<<"lc">>,<<>>,{<<"otter_test">>,{127,0,0,1},0}},
                {<<"initial_tag">>, <<"true">>}
            ]
        }
    ] = finish().


%% Prefilter test for different APIs
ptest_prefilter(_Config) ->
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
    otter_span_pdict_api:log(<<206,177,32,61,58,61,32,207,137>>),
    otter_span_pdict_api:tag("result", "ok"),
    otter_span_pdict_api:finish(),
    [] = finish().

mptest_prefilter(_Config) ->
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
    otter_span_mpdict_api:log("test_span1", <<206,177,32,61,58,61,32,207,137>>),
    otter_span_mpdict_api:tag("test_span1", "result", "ok"),
    otter_span_mpdict_api:finish("test_span1"),
    otter_span_mpdict_api:log("test_span2", "started"),
    otter_span_mpdict_api:log("test_span2", <<206,177,32,61,58,61,32,207,137>>),
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
    otter_span_id_api:log(S1, <<206,177,32,61,58,61,32,207,137>>),
    otter_span_id_api:tag(S1, "result", "ok"),
    otter_span_id_api:finish(S1),
    otter_span_id_api:log(S2, "started"),
    otter_span_id_api:log(S2, <<206,177,32,61,58,61,32,207,137>>),
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


%% Right, comment this test case out until we come up with a better one.
%% This one fails occassionally (i.e. the randomness gets outside the
%% tolerance) and makes travis unhappy ...
%%filter_one_out_of(_Config) ->
%%    L = length([
%%        1 || [ok] <- [
%%            otter_lib_filter:run(
%%                [{tag1, value1}],
%%                [{[{one_out_of, 1000}], [ok]}]
%%            ) || _ <- lists:seq(1, 100000)
%%        ]
%%    ]),
%%    true = if L < 120 andalso L > 80 -> true; true -> false end.

%% Server span handler
handle_span(Span)  ->
    ets:insert(test_span_collector, Span).

custom_http_client_cb(ZipkinURL, Data) ->
    case httpc:request(post, {ZipkinURL, [], "application/x-thrift", Data}, [], []) of
        {ok, {{_, SCode, _}, _, _}} ->
            {ok, SCode};
        Err ->
            Err
    end.

%% Helpers

finish() ->
    timer:sleep(500),
    Result = ets:tab2list(test_span_collector),
    ets:delete_all_objects(test_span_collector),
    Result.

ets_start() ->
    register(test_span_collector, spawn(
        fun() ->
            ets:new(test_span_collector, [named_table, public, {keypos, 2}]),
            receive
                stop ->
                    ok
            end
        end
    )).

ets_stop() ->
    test_span_collector ! stop.

