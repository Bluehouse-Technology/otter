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
%%% the `otter' application.
%%%
%%% The module covers 3 APIs. The `Span API', `Snapshot/Count API' and
%%% `Config API'.
%%%
%%% == Span API ==
%%%
%%% The module exposes the functional span API.
%%% These API calls allow you to create and manage spans. These are:<br/>
%%% `start/1, start/2, start/3'<br/>
%%% `start_with_tags/2, start_with_tags/3, start_with_tags/4'<br/>
%%% `finish/1' <br/>
%%% `ids/1' <br/>
%%% `log/2, log/3' <br/>
%%% `tag/3, tag/4' <br/>
%%%
%%% The API supports pre-filtering of spans with the start_with_tags
%%% functions. Pre-filtering is invoked when the span is started with
%%% initial tags. The result of the pre filter rules can be an inactive
%%% span. Inactive spans do not trigger the rules when finishing the span
%%% (i.e. always discarded) and consume somewhat less resources (depending
%%% on the API) than gathering active spans.
%%%
%%% There are additional span APIs developed for specific use cases to
%%% simplify instrumentations for different code bases. These
%%% are accessible in separate modules.
%%% - simple process dictionary API     : `otter_span_pdict_api' <br/>
%%% - multi span process dictionary API : `otter_span_mpdict_api' <br/>
%%% - span id/process API               : `otter_span_id_api' <br/>
%%% For further information on these APIs, check the documentation of the
%%% modules
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
%%% file. There is an example configuration provided in the `otter.app`
%%% file as a reference. These are:<br/>
%%% `config_list/0'<br/>
%%% `config_read/1'<br/>
%%% `config_read/2'<br/>
%%% `config_write/2'<br/>
%%% @end
%%%-------------------------------------------------------------------

-module(otter).
-include_lib("otter_lib/include/otter.hrl").

-export([
         %% Span API
         start/1, start/2, start/3,
         start_with_tags/2, start_with_tags/3, start_with_tags/4,
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
%% a trace id.
%% @end
%%--------------------------------------------------------------------
-spec start(Name :: info()) -> span().
start(Name) ->
    otter_lib_span:start(Name).

%%--------------------------------------------------------------------
%% @doc Starts a span with the specified name and parent span or a trace id
%% @end
%% --------------------------------------------------------------------
-spec start(Name :: info(), ParentSpan :: span()) -> span().
start(Name, #span{} = ParentSpan) ->
    {TraceId, ParentId} = ids(ParentSpan),
    otter_lib_span:start(Name, TraceId, ParentId);
start(Name, TraceId) when is_integer(TraceId) ->
    otter_lib_span:start(Name, TraceId).

%%--------------------------------------------------------------------
%% @doc Starts a child span with the specified name, trace id and
%% parent id
%% @end
%% --------------------------------------------------------------------
-spec start(Name :: info(), TraceId :: integer(), ParentId :: integer()) -> span().
start(Name, TraceId, ParentId) when is_integer(TraceId),
                                    is_integer(ParentId) ->
    otter_lib_span:start(Name, TraceId, ParentId).


%%--------------------------------------------------------------------
%% @doc Starts a span with the specified name and initial tags.
%% Automatically generates a trace id and invokes pre filtering.
%% @end
%%--------------------------------------------------------------------
%% TODO Figure out why dialyzer doesn't like this spec : -spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().
start_with_tags(Name, Tags) ->
    Span = otter_lib_span:start(Name, Tags),
    otter_filter:pre_span(Span).

%%--------------------------------------------------------------------
%% @doc Starts a span with the specified name, initial tags and
%% trace id or a parent span
%% @end
%% --------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()], Span :: span()) -> span();
                     (Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span().
start_with_tags(Name, Tags, #span{trace_id = TraceId, id = ParentId}) ->
    Span = otter_lib_span:start_with_tags(Name, Tags, TraceId, ParentId),
    otter_filter:pre_span(Span);


start_with_tags(Name, Tags, TraceId) when is_integer(TraceId) ->
    Span = otter_lib_span:start_with_tags(Name, Tags, TraceId),
    otter_filter:pre_span(Span).


%%--------------------------------------------------------------------
%% @doc Starts a child span with the specified name, trace id and
%% parent id
%% @end
%% --------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id(), ParentId :: span_id()) -> span().
start_with_tags(Name, Tags, TraceId, ParentId) when is_integer(TraceId),
                                                    is_integer(ParentId) ->
    Span = otter_lib_span:start_with_tags(Name, Tags, TraceId, ParentId),
    otter_filter:pre_span(Span).

%%--------------------------------------------------------------------
%% @doc Adds a tag to a span. If the tag already exists, its value
%% will be overwritten.
%% @end
%% --------------------------------------------------------------------
-spec tag(Span :: span(), Key :: info(), Value :: info()) -> span().
tag(#span{timestamp = 0} = Span, _Key, _Value) ->
    Span;
tag(#span{} = Span, Key, Value) ->
    otter_lib_span:tag(Span, Key, Value).

%%--------------------------------------------------------------------
%% @doc Adds a tag to a span with a given service. If the tag already
%% exists, its value will be overwritten.
%% @end
%% --------------------------------------------------------------------
-spec tag(Span :: span(), Key :: info(), Value :: info(), Service :: service()) -> span().
tag(#span{timestamp = 0} = Span, _Key, _Value, _Service) ->
    Span;
tag(#span{} = Span, Key, Value, Service) ->
    otter_lib_span:tag(Span, Key, Value, Service).

%%--------------------------------------------------------------------
%% @doc Adds a log message to a span.  If the span is not active,
%% the tag is not added.
%% @end
%% --------------------------------------------------------------------
-spec log(Span :: span(), Text :: info()) -> span().
log(#span{timestamp = 0} = Span, _Text) ->
    Span;
log(#span{} = Span, Text) ->
    otter_lib_span:log(Span, Text).

%%--------------------------------------------------------------------
%% @doc Adds a log message to a span with the specified service information.
%% If the span is not active, the tag is not added.
%% @end
%% --------------------------------------------------------------------
-spec log(Span :: span(), Text :: info(), Service :: service()) -> span().
log(#span{timestamp = 0} = Span, _Text, _Service) ->
    Span;
log(#span{} = Span, Text, Service) ->
    otter_lib_span:log(Span, Text, Service).

%%--------------------------------------------------------------------
%% @doc Ends an active span and prepares it for potential delivery to the
%% backend based on filtering rules.
%% @end
%% --------------------------------------------------------------------
-spec finish(Span :: span()) -> ok.
finish(#span{timestamp = 0}) ->
    ok;
finish(#span{} = Span) ->
    otter_filter:span(otter_lib_span:finish(Span)).


%%--------------------------------------------------------------------
%% @doc Returns the trace id and span id for a given span.
%% @end
%% --------------------------------------------------------------------
-spec ids(Span :: span()) -> {trace_id(), span_id()}.
ids(#span{timestamp = 0}) ->
    %% Not sure what sensible things can we return here. I think the main
    %% consideration would be not to crash code that expect integers ...
    {0,0};
ids(#span{} = Span) ->
    otter_lib_span:get_ids(Span).

%%--------------------------------------------------------------------
%% Snapshot/Counter API
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc List all the snapshot counters
%% @end
%% --------------------------------------------------------------------
-spec counter_list() -> [{term(), integer()}].
counter_list() ->
    otter_lib_snapshot_count:list_counts().

%%--------------------------------------------------------------------
%% @doc Show the last event snapshot of a counter
%% @end
%% --------------------------------------------------------------------
-spec counter_snapshot(Key :: term()) -> term().
counter_snapshot(Key) ->
    otter_lib_snapshot_count:get_snap(Key).

%%--------------------------------------------------------------------
%% @doc Delete (reset) a counter
%% @end
%% --------------------------------------------------------------------
-spec counter_delete(Key :: term()) -> ok.
counter_delete(Key) ->
    otter_lib_snapshot_count:delete_counter(Key).

%%--------------------------------------------------------------------
%% @doc Delete (reset) all counters
%% @end
%% --------------------------------------------------------------------
-spec counter_delete_all() -> ok.
counter_delete_all() ->
    otter_lib_snapshot_count:delete_all_counters().

%%--------------------------------------------------------------------
%% Config API
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc List all defined configuration
%% @end
%% --------------------------------------------------------------------
-spec config_list() -> term().
config_list() ->
    otter_config:list().

%%--------------------------------------------------------------------
%% @doc Read a configuration value
%% @end
%% --------------------------------------------------------------------
-spec config_read(Key :: atom()) -> term().
config_read(Key) ->
    otter_config:read(Key).

%%--------------------------------------------------------------------
%% @doc Read a configuration value with default
%% @end
%% --------------------------------------------------------------------
-spec config_read(Key :: atom(), Default :: term()) -> term().
config_read(Key, Default) ->
    otter_config:read(Key, Default).


%%--------------------------------------------------------------------
%% @doc Write a configuration value.
%% Note : With the default config implementation this write is not
%% persistent. Update the application environment/config accordingly to
%% persist.
%% @end
%% --------------------------------------------------------------------
-spec config_write(Key :: atom(), Value :: term()) -> ok.
config_write(Key, Value) ->
    otter_config:write(Key, Value).

