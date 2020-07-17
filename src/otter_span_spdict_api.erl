%%%-------------------------------------------------------------------
%%% @author guru vishnu
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
%%% and can be used when all span tags and events happen in the same
%%% request handling process. Multiple spans can be stored with
%%% different names stored in stack(Last In First Out) format.
%%% This API allows creating nested spans for better tracing.
%%% After starting the span, subsequent operations (e.g.
%%% tag, log, finish) will automatically apply on the latest span.
%%% The API supports pre filtering with the start_with_tags functions.
%%% @end
%%%-------------------------------------------------------------------


-module(otter_span_spdict_api).
-author("guruvishnu").
-export([
    start/1, start_with_tags/2,
    finish/0,
    log/1, tag/2
]).

-include_lib("otter_lib/include/otter.hrl").


%%----------------------------------------------------------------------
%% @doc start a new span on the process dictionary of the current process
%% New span will be pushed on top of the stack and all tag/log operations
%% will be performed on it.
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info()) -> span().
start(Name) ->
    start_with_tags(Name, []).

%%----------------------------------------------------------------------
%% @doc start a new span with a list of tags on the process
%% dictionary of the current process and invoke the pre filter
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().
start_with_tags(Name, Tags) ->
    Span =
        case stack_top() of
            #span{trace_id = TraceId, parent_id = ParentId} ->
                otter_lib_span:start_with_tags(Name, Tags, TraceId, ParentId);
            _ ->
                otter_lib_span:start_with_tags(Name, Tags)
        end,
    stack_push(Span),
    Span.


%%----------------------------------------------------------------------
%% @doc Add a tag to the span in the process dictionary. If the span is
%% not active, a fake/empty span is returned with the default values and
%% timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec tag(Key :: info(), Value :: info()) -> span().
tag(Key, Value) ->
    case stack_pop() of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:tag(Span, Key, Value),
            stack_push(NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.


%%----------------------------------------------------------------------
%% @doc Add a log to the span in the process dictionary. If the span is
%% not active, a "fake" span is returned with timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec log(Text :: info()) -> span().
log(Text) ->
    case stack_pop() of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:log(Span, Text),
            stack_push(NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.


%%----------------------------------------------------------------------
%% @doc Finish collection of span information and invoke the span filter
%% unless the span is marked as inactive (timestamp set to 0)
%% Latest span will be finished in last in first out format
%% @end
%%----------------------------------------------------------------------
-spec finish() -> ok.
finish() ->
    case stack_pop() of
        Span when Span#span.timestamp =/= 0 ->
            otter_filter:span(otter_lib_span:finish(Span));
        _ ->
            ok
    end.

%%----------------------------------------------------------------------
%% Internal helper functions for stack operations
%%----------------------------------------------------------------------

-spec stack_top() -> span() | error.
stack_top() ->
    case get(otter_span_information_stack) of
        SpanStack when is_list(SpanStack) andalso length(SpanStack) =/= 0 ->
            [TopSpan | _RestSpan] = SpanStack,
            TopSpan;
        _ ->
            error
    end.

-spec stack_push(span()) -> undefined.
stack_push(Span) ->
    case get(otter_span_information_stack) of
        undefined ->
            put(otter_span_information_stack, [Span]);
        SpanStack ->
            put(otter_span_information_stack, [Span | SpanStack])
    end.

-spec stack_pop() -> span() | error.
stack_pop() ->
    case get(otter_span_information_stack) of
        SpanStack when is_list(SpanStack) andalso length(SpanStack) =/= 0 ->
            [TopSpan | RestSpan] = SpanStack,
            put(otter_span_information_stack, RestSpan),
            TopSpan;
        _ ->
            error
    end.

