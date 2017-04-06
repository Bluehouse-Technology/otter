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
%%% otter API module
%%% @end
%%%-------------------------------------------------------------------

-module(otter).
-compile(export_all).
-include("otter.hrl").

%% ====================  SPAN function API  ======================
%% This API functions with passing around the Span in the function calls
%% All of them return a Span structure (erlang map).

-spec span_start(info()) -> span().
span_start(Name) ->
    otter_span:fstart(Name).

-spec span_start(info(), integer()) -> span().
span_start(Name, TraceId)
  when is_integer(TraceId) ->
    otter_span:fstart(Name, TraceId).

-spec span_start(info(), integer(), integer()) -> span().
span_start(Name, TraceId, ParentId)
  when is_integer(TraceId), is_integer(ParentId) ->
    otter_span:fstart(Name, TraceId, ParentId).

-spec span_tag(span(), info(), info()) -> span().
span_tag(Span, Key, Value)
  when is_record(Span, span) ->
    otter_span:ftag(Span, Key, Value).

-spec span_tag(span(), info(), info(), service()) -> span().
span_tag(Span, Key, Value, Service)
  when is_record(Span, span) ->
    otter_span:ftag(Span, Key, Value, Service).


-spec span_log(span(), info()) -> span().
span_log(Span, Text)
  when is_record(Span, span) ->
    otter_span:flog(Span, Text).

-spec span_log(span(), info(), service()) -> span().
span_log(Span, Text, Service)
  when is_record(Span, span) ->
    otter_span:flog(Span, Text, Service).

-spec span_end(span()) -> ok.
span_end(Span)
  when is_record(Span, span) ->
    otter_span:fend(Span).

-spec span_ids(span()) -> {trace_id(), span_id()}.
span_ids(Span)
  when is_record(Span, span) ->
    otter_span:fget_ids(Span).


%% ====================  SPAN process API  ======================
%% This API uses the process dictionary to collect span information
%% and can be used when all span tags an events happen in the same
%% request handling process.

-spec span_pstart(info()) -> ok.
span_pstart(Name) ->
    otter_span:pstart(Name).

-spec span_pstart(info(), trace_id()) -> ok.
span_pstart(Name, TraceId) ->
    otter_span:pstart(Name, TraceId).

-spec span_pstart(info(), trace_id(), span_id()) -> ok.
span_pstart(Name, TraceId, ParentId) ->
    otter_span:pstart(Name, TraceId, ParentId).

-spec span_ptag(info(), info()) -> ok.
span_ptag(Key, Value) ->
    otter_span:ptag(Key, Value).

-spec span_ptag(info(), info(), service()) -> ok.
span_ptag(Key, Value, Service) ->
    otter_span:ptag(Key, Value, Service).

-spec span_plog(info()) -> ok.
span_plog(Text) ->
    otter_span:plog(Text).

-spec span_plog(info(), service()) -> ok.
span_plog(Text, Service) ->
    otter_span:plog(Text, Service).

-spec span_pend() -> ok.
span_pend() ->
    otter_span:pend().

-spec span_pids() -> {trace_id(), span_id()}.
span_pids() ->
    otter_span:pget_ids().

-spec span_pget() -> span().
span_pget() ->
    otter_span:pget_span().



%% ========================  Snap/Count API  =========================
%% When span_end/1 or span_pend/0 is called then the completed span is
%% passed to a configurable filter. The filter can check the Span tags
%% as well as the name and duration of the span and use the information
%% to decide to send the Span to the trace collector (Zipkin supported)
%% and/or increase counters based on values of the tags and store the
%% last Span for the counters. This latter is particularly useful for
%% troubleshooting e.g. error events when increase of the corresponding
%% counter is noticed. These snapshots (referred as Snap) and counters
%% can be retrieved, managed with this API

-spec counter_list() -> [{list(), integer()}].
counter_list() ->
    otter_snapshot_count:list_counts().

-spec counter_snapshot(list()) -> term().
counter_snapshot(Key) ->
    otter_snapshot_count:get_snap(Key).

-spec counter_delete(list()) -> ok.
counter_delete(Key) ->
    otter_snapshot_count:delete_counter(Key).

-spec counter_delete_all() -> ok.
counter_delete_all() ->
    otter_snapshot_count:delete_all_counters().


%% ========================== Config API ============================
%% The default implementation uses the application environment to
%% store configuration. There is a simple wrapper module to interface
%% with configuration store (otter_config). To implementat other config
%% persistence, the module should be replaced with another one providing
%% the same simple read/write API functions.
%% WARNING : In the default implementation using the application
%% environment, so the write function is NOT persistent. In case of node
%% restart and/or application reload the configuration will be reset to
%% whatever environment is defined in the release (sys) config or app
%% file. There is an example configuration provided in the otter.app
%% file as a reference.

-spec config_list() -> term().
config_list() ->
    otter_config:list().

-spec config_read(atom()) -> term().
config_read(Key) ->
    otter_config:read(Key).

-spec config_read(atom(), term()) -> term().
config_read(Key, Default) ->
    otter_config:read(Key, Default).

-spec config_write(atom(), term()) -> ok.
config_write(Key, Value) ->
    otter_config:write(Key, Value).

