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

%%% @doc otter API module. Use the API in this module to interact with
%%% the `otter' application. If you want to specifically use the
%%% process dictionary API, use the `otter_pdict_api' module instead.
%%% The API is broken down into 3 sections. The `Span API',
%%% `Snapshot/Count API' and `Config API'.
%%%
%%% == Span API ==
%%% These API calls allow you to create and manage spans. These are:<br/>
%%% `start/1, start/2, start/3'<br/>
%%% `finish/1' <br/>
%%% `ids/1' <br/>
%%% `log/2, log/3' <br/>
%%% `tag/3, tag/4' <br/>
%%%
%%% == Snapshot/Count API  ==
%%% When `finish/1' is called then the completed span is
%%% passed to a configurable filter. The filter can check the Span tags
%%% as well as the name and duration of the span and use the information
%%% to decide to send the Span to the trace collector (Zipkin supported)
%%% and/or increase counters based on values of the tags and store the
%%% last Span for the counters. This latter is particularly useful for
%%% troubleshooting e.g. error events when increase of the corresponding
%%% counter is noticed. These snapshots (referred as Snap) and counters
%%% can be retrieved, managed with this API. These are:<br/>
%%% `counter_list/0'<br/>
%%% `counter_snapshot/1'<br/>
%%% `counter_delete/1'<br/>
%%% `counter_delete_all/0'<br/>
%%% 
%%% == Config API ==
%%% The default implementation uses the application environment to
%%% store configuration. There is a simple wrapper module to interface
%%% with configuration store (otter_config). To implement other config
%%% persistence, the module should be replaced with another one providing
%%% the same simple read/write API functions.<br/>
%%% <em>WARNING</em> : In the default implementation using the application
%%% environment, so the write function is NOT persistent. In case of node
%%% restart and/or application reload the configuration will be reset to
%%% whatever environment is defined in the release (sys) config or app
%%% file. There is an example configuration provided in the `otter.app'
%%% file as a reference. These are:<br/>
%%% `config_list/0'<br/>
%%% `config_read/1'<br/>
%%% `config_read/2'<br/>
%%% `config_write/2'<br/>
%%% @end
%%%-------------------------------------------------------------------

-module(otter).
-include("otter.hrl").

-export([
         %% Span API
         start/1, start/2, start/3,
         finish/1,
         ids/1,
         log/2, log/3,
         tag/3, tag/4,

         %% Snapshot / Count API
         counter_list/0,
         counter_snapshot/1,
         counter_delete/1,
         counter_delete_all/0,

         %% Config API
         config_list/0,
         config_read/1,
         config_read/2,
         config_write/2
        ]).

%%--------------------------------------------------------------------
%% @doc Starts a span with the specified name. Automatically generates
%% a TraceId.  
%% @end
%%--------------------------------------------------------------------
-spec start(info()) -> span().
start(Name) ->
    otter_span:fstart(Name).

%%--------------------------------------------------------------------
%% @doc Starts a span with the specified name and TraceId
%% @end
%%--------------------------------------------------------------------
-spec start(info(), integer()) -> span().
start(Name, TraceId) ->
    otter_span:fstart(Name, TraceId).

%%--------------------------------------------------------------------
%% @doc Starts a child span with the specified name, TraceId and
%% ParentId 
%% @end
%% --------------------------------------------------------------------
-spec start(info(), integer(), integer()) -> span().
start(Name, TraceId, ParentId) ->
    otter_span:fstart(Name, TraceId, ParentId).

%%--------------------------------------------------------------------
%% @doc Adds a tag to a span. If the tag already exists, its value
%% will be overwritten 
%% @end
%% --------------------------------------------------------------------
-spec tag(span(), info(), info()) -> span().
tag(Span, Key, Value) ->
    otter_span:ftag(Span, Key, Value).

%%--------------------------------------------------------------------
%% @doc Adds a tag to a span with a given service. If the tag already
%% exists, its value will be overwritten
%% @end
%% --------------------------------------------------------------------
-spec tag(span(), info(), info(), service()) -> span().
tag(Span, Key, Value, Service) ->
    otter_span:ftag(Span, Key, Value, Service).

%%--------------------------------------------------------------------
%% @doc Adds a log message to a span
%% @end
%% --------------------------------------------------------------------
-spec log(span(), info()) -> span().
log(Span, Text) ->
    otter_span:flog(Span, Text).

-spec log(span(), info(), service()) -> span().
log(Span, Text, Service) ->
    otter_span:flog(Span, Text, Service).

%%--------------------------------------------------------------------
%% @doc Ends a span and prepares it for potential delivery to the
%% backend based on filtering rules
%% @end
%% --------------------------------------------------------------------
-spec finish(span()) -> ok.
finish(Span) ->
    otter_span:fend(Span).

%%--------------------------------------------------------------------
%% @doc Returns the TraceId and SpanId for a given span.
%% @end
%% --------------------------------------------------------------------
-spec ids(span()) -> {trace_id(), span_id()}.
ids(Span) ->
    otter_span:fget_ids(Span).


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

