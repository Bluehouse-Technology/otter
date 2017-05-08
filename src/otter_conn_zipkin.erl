%%%-------------------------------------------------------------------
%%% Licensed to the Apache Software Foundation (ASF) under one
%%% or more contributor license agreements.  See the NOTICE file
%%% distributed with this work for additional information
%%% regarding copyright ownership.  The ASF licenses this file
%%% to you under the Apache License, Version 2.0 (the
%%% "License"); you may not use this file except in compliance
%%% with the License.  You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% @doc This module facilitates encoding/decoding of thrift data
%%% lists encoded with the binary protocol, suitable for
%%% sending/receiving spans on the zipkin interface. Sending spans to
%%% Zipkin is async
%%% @end
%%% -------------------------------------------------------------------

-module(otter_conn_zipkin).
-export([
         send_buffer/0,
         store_span/1,
         sup_init/0
        ]).

sup_init() ->
    [
        ets:new(
            Tab,
            [named_table, public | TableProps ]
        ) ||
        {Tab, TableProps} <- [
            {otter_zipkin_buffer1, [{write_concurrency, true}, {keypos, 2}]},
            {otter_zipkin_buffer2, [{write_concurrency, true}, {keypos, 2}]},
            {otter_zipkin_status,  [{read_concurrency, true}]}
        ]
    ],
    ets:insert(otter_zipkin_status, {current_buffer, otter_zipkin_buffer1}),
    SendInterval = otter_config:read(zipkin_batch_interval_ms, 100),
    timer:apply_interval(SendInterval, ?MODULE, send_buffer, []).

store_span(Span) ->
    [{_, Buffer}] = ets:lookup(otter_zipkin_status, current_buffer),
    ets:insert(Buffer, Span).

send_buffer() ->
    [{_, Buffer}] = ets:lookup(otter_zipkin_status, current_buffer),
    NewBuffer = case Buffer of
        otter_zipkin_buffer1 ->
            otter_zipkin_buffer2;
        otter_zipkin_buffer2 ->
            otter_zipkin_buffer1
    end,
    ets:insert(otter_zipkin_status, {current_buffer, NewBuffer}),
    case ets:tab2list(Buffer) of
        [] ->
            ok;
        Spans ->
            ets:delete_all_objects(Buffer),
            case send_batch_to_zipkin(Spans) of
                {ok, 202} ->
                    otter_lib_snapshot_count:snapshot(
                        [?MODULE, send_buffer, ok],
                        [{spans, length(Spans)}]);
                Error ->
                    otter_lib_snapshot_count:snapshot(
                        [?MODULE, send_buffer, failed],
                        [
                            {spans, length(Spans)},
                            {error, Error}
                        ])
            end
    end.

send_batch_to_zipkin(Spans) ->
    {ok, ZipkinURL} = otter_config:read(zipkin_collector_uri),
    send_batch_to_zipkin(ZipkinURL, Spans).

send_batch_to_zipkin(ZipkinURL, Spans) ->
    ExtraTags = case otter_config:read(zipkin_add_host_tag_to_span, undefined) of
        {Key, Value} ->
            [{Key, Value, default}];
        _ ->
            []
    end,
    DefaultService = otter_config:read(
        zipkin_tag_host_service,
        node()
    ),
    ServiceDefaults = {
        otter_lib:to_bin(DefaultService),
        otter_config:read(zipkin_tag_host_ip, {127,0,0,1}),
        otter_config:read(zipkin_tag_host_port, 0)
    },
    Data = otter_lib_zipkin_thrift:encode_spans(Spans, ExtraTags, ServiceDefaults),
    send_spans_http(ZipkinURL, Data).

send_spans_http(ZipkinURL, Data) ->
    send_spans_http(otter_config:read(http_client, ibrowse), ZipkinURL, Data).

send_spans_http(ibrowse, ZipkinURL, Data) ->
    case ibrowse:send_req(
        ZipkinURL,
        [{"content-type", "application/x-thrift"}],
        post,
        Data
     ) of
    {ok, SCode, _, _} ->
        {ok, list_to_integer(SCode)};
    Err ->
        Err
    end;
send_spans_http(httpc, ZipkinURL, Data) ->
    case httpc:request(post, {ZipkinURL, [], "application/x-thrift", Data}, [], []) of
    {ok, {{_, SCode, _}, _, _}} ->
        {ok, SCode};
    Err ->
        Err
    end.
