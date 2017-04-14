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
%%% This API uses the process dictionary to collect span information
%%% and can be used when all span tags an events happen in the same
%%% request handling process.
%%% @end
%%%-------------------------------------------------------------------

-module(otter_span_pdict_api).
-export([
         start/1, start/2, start/3,
         finish/0, finish/1,
         get/0,
         ids/0, ids/1,
         log/1, log/2,
         tag/2, tag/3
        ]).

-include("otter.hrl").

-spec start(info()) -> span().
start(Name) ->
    otter_span:pstart(Name).

-spec start(info(), trace_id()) -> span().
start(Name, TraceId) ->
    otter_span:pstart(Name, TraceId).

-spec start(info(), trace_id(), span_id()) -> span().
start(Name, TraceId, ParentId) ->
    otter_span:pstart(Name, TraceId, ParentId).

-spec tag(info(), info()) -> span().
tag(Key, Value) ->
    otter_span:ptag(Key, Value).

-spec tag(info(), info(), service()) -> span().
tag(Key, Value, Service) ->
    otter_span:ptag(Key, Value, Service).

-spec log(info()) -> ok.
log(Text) ->
    otter_span:plog(Text).

-spec log(info(), service()) -> ok.
log(Text, Service) ->
    otter_span:plog(Text, Service).

-spec finish() -> ok.
finish() ->
    otter_span:pend().

-spec finish(span()) -> ok.
%% @doc This is provided purely for API compatibility with the otter_span_api module
finish(_Span) ->
    otter_span:pend().

-spec ids() -> {trace_id(), span_id()}.
ids() ->
    otter_span:pget_ids().

-spec ids(span()) -> {trace_id(), span_id()}.
%% @doc This is provided purely for API compatibility with the otter_span_api module
ids(_Span) ->
    otter_span:pget_ids().

-spec get() -> span().
get() ->
    otter_span:pget_span().
