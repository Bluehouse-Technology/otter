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
%%% This API uses a separate process to collect information of a span. The
%%% idea behind this is to provide a static ID which can be used to refer
%%% to the span. This is more convenient to integrate than the basic
%%% functional API is not limited by process boundaries as with the process
%%% dictionary APIs. The API also supports pre filtering with the
%%% start_with_tags functions.
%%% @end
%%%-------------------------------------------------------------------

-module(otter_span_id_api).
-export([
    start/1, start/2, start/3,
    start_with_tags/2, start_with_tags/3, start_with_tags/4,
    finish/1,
    ids/1,
    log/2, log/3,
    tag/3, tag/4
]).

-include_lib("otter_lib/src/otter.hrl").

%%----------------------------------------------------------------------
%% @doc Start a new span collection process
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info()) -> pid().
start(Name) ->
    span_process(otter_lib_span:start(Name)).

%%----------------------------------------------------------------------
%% @doc Start a new span collection process with an existing trace id or
%% parent span
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info(), TraceId :: trace_id()) -> pid();
           (Name :: info(), ParentSpan :: span()) -> pid().
start(Name, #span{trace_id = TraceId, id = ParentId}) ->
    span_process(otter_lib_span:start(Name, TraceId, ParentId));
start(Name, TraceId) when is_integer(TraceId) ->
    span_process(otter_lib_span:start(Name, TraceId)).

%%----------------------------------------------------------------------
%% @doc Start a new span collection process with an existing trace id and
%% parent span id
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: integer()) -> pid().
start(Name, TraceId, ParentId) when is_integer(TraceId), is_integer(ParentId) ->
    span_process(otter_lib_span:start(Name, TraceId, ParentId)).


%%----------------------------------------------------------------------
%% @doc Start a new span with a list of tags and pass it to pre filtering.
%% If the span is active then start a span collection process. For inactive
%% span the atom `undefined' is returned instead of the PID.
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [{info(), info()}]) -> span().
start_with_tags(Name, Tags) ->
    Span = otter_lib_span:start_with_tags(Name, Tags),
    case otter_filter:pre_span(Span) of
        NewSpan when ?is_span_active(NewSpan) ->
            span_process(NewSpan);
        _ ->
            undefined
    end.

%%----------------------------------------------------------------------
%% @doc Start a new span with a list of tags and a trace id or parent span
%% and pass it to pre filtering.
%% If the span is active then start a span collection process. For inactive
%% span the atom `undefined' is returned instead of the PID.
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), TraceId :: trace_id(), Tags :: [{info(), info()}]) -> span();
                     (Name :: info(), ParentSpan :: span(), Tags :: [{info(), info()}]) -> span().
start_with_tags(Name, #span{trace_id = TraceId, id = ParentId}, Tags) ->
    Span = otter_lib_span:start_with_tags(Name, TraceId, ParentId, Tags),
    case otter_filter:pre_span(Span) of
        NewSpan when ?is_span_active(NewSpan) ->
            span_process(NewSpan);
        _ ->
            undefined
    end;
start_with_tags(Name, TraceId, Tags) when is_integer(TraceId) ->
    Span = otter_lib_span:start_with_tags(Name, TraceId, Tags),
    case otter_filter:pre_span(Span) of
        NewSpan when ?is_span_active(NewSpan) ->
            span_process(NewSpan);
        _ ->
            undefined
    end.


%%----------------------------------------------------------------------
%% @doc Start a new span with a list of tags, a trace id and parent span id
%% then pass it to pre filtering.
%% If the span is active then start a span collection process. For inactive
%% span the atom `undefined' is returned instead of the PID.
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), TraceId :: trace_id(), ParentId :: span_id(), Tags :: [{info(), info()}]) -> span().
start_with_tags(Name, TraceId, ParentId, Tags) when is_integer(TraceId), is_integer(ParentId) ->
    Span = otter_lib_span:start_with_tags(Name, TraceId, ParentId, Tags),
    case otter_filter:pre_span(Span) of
        NewSpan when ?is_span_active(NewSpan) ->
            span_process(NewSpan);
        _ ->
            undefined
    end.

%%----------------------------------------------------------------------
%% @doc Add a tag to the span
%% @end
%%----------------------------------------------------------------------
-spec tag(Pid :: pid()|undefined, Key :: info(), Value :: info()) -> ok.
tag(undefined, _Key, _Value) ->
    ok;
tag(Pid, Key, Value) ->
    Pid ! {tag, Key, Value},
    ok.

%%----------------------------------------------------------------------
%% @doc Add a tag to the span with specific service information
%% @end
%%----------------------------------------------------------------------
-spec tag(Pid :: pid()|undefined, Key :: info(), Value :: info(), Service :: service()) -> ok.
tag(undefined, _Key, _Value, _Service) ->
    ok;
tag(Pid, Key, Value, Service) ->
    Pid ! {tag, Key, Value, Service},
    ok.

%%----------------------------------------------------------------------
%% @doc Add a log to the span
%% @end
%%----------------------------------------------------------------------
-spec log(Pid :: pid()|undefined, Text :: info()) -> ok.
log(undefined, _Text) ->
    ok;
log(Pid, Text) ->
    Pid ! {log, Text},
    ok.

%%----------------------------------------------------------------------
%% @doc Add a log to the span with specific service information
%% @end
%%----------------------------------------------------------------------
-spec log(Pid :: pid()|undefined, Text :: info(), Service :: service()) -> ok.
log(undefined, _Text, _Service) ->
    ok;
log(Pid, Text, Service) ->
    Pid ! {log, Text, Service},
    ok.

%%----------------------------------------------------------------------
%% @doc Finish the span and pass it to filtering if it is active
%% @end
%%----------------------------------------------------------------------
-spec finish(Pid :: pid()|undefined) -> ok.
finish(undefined) ->
    ok;
finish(Pid) ->
    Pid ! finish,
    ok.

%%----------------------------------------------------------------------
%% @doc Get the trace id and span id from a span. If the span is inactive
%% it returns the tuple `{0,0}'
%% @end
%%----------------------------------------------------------------------
-spec ids(Pid :: pid()|undefined) -> {trace_id(), span_id()}.
ids(undefined) ->
    {0, 0};
ids(Pid) ->
    Pid ! {get_ids, self()},
    receive
        {otter_span_ids, Ids} -> Ids
    after
        %% Actually I should in general (re)consider the error handling
        %% of otter : probably we'd be better off crashing in the span
        %% encoding phase due to silly data, generated by errors, than
        %% crashing the request handling process we are tracing ...
        %% So considering this we'll return {0, 0} and perhaps we should
        %% add a counter to keep track of these errors
        300 ->
            otter_lib_snapshot_count:snapshot(otter_span_mpdict_api_get_id_timeout, []),
            {0, 0}
    end.


%%----------------------------------------------------------------------
%% Internal helper
%%----------------------------------------------------------------------

span_process(Span) ->
    Timeout = otter_config:read(span_id_api_process_timeout, 30000),
    spawn(fun() -> span_process(Span, Timeout) end).

span_process(Span, Timeout) ->
    receive
        {tag, Key, Value} ->
            span_process(otter_lib_span:tag(Span, Key, Value), Timeout);
        {tag, Key, Value, Service} ->
            span_process(otter_lib_span:tag(Span, Key, Value, Service), Timeout);
        {log, Text} ->
            span_process(otter_lib_span:log(Span, Text), Timeout);
        {log, Text, Service} ->
            span_process(otter_lib_span:log(Span, Text, Service), Timeout);
        {get_ids, Pid} ->
            Pid ! {otter_span_ids, otter_lib_span:get_ids(Span)},
            span_process(Span, Timeout);
        finish ->
            otter_filter:span(otter_lib_span:finish(Span))
    after
        Timeout ->
            ok
    end.



