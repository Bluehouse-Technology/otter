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

-module(otter_span_mpdict_api).
-export([
         start/1, start/2, start/3,
         finish/1,
         get_span/1,
         put_span/1,
         ids/1,
         log/2, log/3,
         tag/3, tag/4
        ]).

-include_lib("otter_lib/src/otter.hrl").

-spec start(info()) -> span().
start(Name) ->
    Span = otter_lib_span:start(Name),
    put({otter_span_information, Name}, Span),
    Span.


-spec start(info(), trace_id()) -> span().
start(Name, TraceId) when is_integer(TraceId) ->
    Span = otter_lib_span:start(Name, TraceId),
    put({otter_span_information, Name}, Span),
    Span.

-spec start(info(), trace_id(), span_id()) -> span().
start(Name, TraceId, ParentId) when is_integer(TraceId), is_integer(ParentId) ->
    Span = otter_lib_span:start(Name, TraceId, ParentId),
    put({otter_span_information, Name}, Span),
    Span.

-spec tag(info(), info(), info()) -> span().
tag(Name, Key, Value) ->
    Span = otter_lib_span:tag(get({otter_span_information, Name}), Key, Value),
    put({otter_span_information, Name}, Span),
    Span.

-spec tag(info(), info(), info(), service()) -> span().
tag(Name, Key, Value, Service) ->
    Span = otter_lib_span:tag(get({otter_span_information, Name}), Key, Value, Service),
    put({otter_span_information, Name}, Span),
    Span.

-spec log(info(), info()) -> span().
log(Name, Text) ->
    Span = otter_lib_span:log(get({otter_span_information, Name}), Text),
    put({otter_span_information, Name}, Span),
    Span.

-spec log(info(), info(), service()) -> span().
log(Name, Text, Service) ->
    Span = otter_lib_span:log(get({otter_span_information, Name}), Text, Service),
    put({otter_span_information, Name}, Span),
    Span.

-spec finish(info()) -> ok.
finish(Name) ->
    otter_filter:span(otter_lib_span:finish(get({otter_span_information, Name}))).

-spec ids(info()) -> {trace_id(), span_id()}.
ids(Name) ->
    otter_lib_span:get_ids(get({otter_span_information, Name})).

-spec get_span(info()) -> span().
get_span(Name) ->
    get({otter_span_information, Name}).

-spec put_span(span()) -> term().
put_span(Span) ->
    put({otter_span_information, otter_lib_span:get_name(Span)}, Span).
