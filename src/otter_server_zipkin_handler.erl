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
%%% @doc
%%% This module is used purely for test purposes.
%%% @end
%%%-------------------------------------------------------------------

-module(otter_server_zipkin_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, {CbMod, CbFun}} = otter_config:read(server_zipkin_callback),
    {ok, Body, Req1} = cowboy_req:body(Req),
    SpanList = otter_conn_zipkin:decode_spans(Body),
    [CbMod:CbFun(Span) || Span <- SpanList],
    {ok, Req2} = cowboy_req:reply(
        202,
        [
            {<<"X-Application-Context">>, <<"zipkin-server:shared:9411">>}
        ],
        <<>>,
        Req1
    ),
    {ok, Req2, State}.

terminate(_, _, _) ->
    ok.
