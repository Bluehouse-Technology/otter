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
%%% request handling process. There is only 1 span managed with this API.
%%% Pre-filtering is supported with the start_with_tags functions.
%%%
%%% If you need more spans managed on the process dictionary, then look
%%% at the 'otter_span_mpdict_api' API module which can manage multiple
%%% spans with different names.
%%% @end
%%%-------------------------------------------------------------------

-module(otter_span_pdict_api).
-export([
         start/1, start/2, start/3,
         start_with_tags/2, start_with_tags/3, start_with_tags/4,
         finish/0,
         get_span/0,
         put_span/1,
         ids/0,
         log/1, log/2,
         tag/2, tag/3
        ]).

-include_lib("otter_lib/include/otter.hrl").

%%----------------------------------------------------------------------
%% @doc start a new span on the process dictionary of the current process
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info()) -> span().
start(Name) ->
    Span = otter_lib_span:start(Name),
    put(otter_span_information, Span),
    Span.

%%----------------------------------------------------------------------
%% @doc start a new span on the process dictionary of the current process
%% with an existing trace id or with a parent span
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info(), TraceId :: trace_id()) -> span();
           (Name :: info(), ParentSpan :: span()) -> span().
start(Name, #span{trace_id = TraceId, id = ParentId}) ->
    Span = otter_lib_span:start(Name, TraceId, ParentId),
    put(otter_span_information, Span),
    Span;
start(Name, TraceId) when is_integer(TraceId) ->
    Span = otter_lib_span:start(Name, TraceId),
    put(otter_span_information, Span),
    Span.

%%----------------------------------------------------------------------
%% @doc start a new span on the process dictionary of the current process
%% with an existing trace id and parent id
%% @end
%%----------------------------------------------------------------------
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: span_id()) -> span().
start(Name, TraceId, ParentId) when is_integer(TraceId), is_integer(ParentId) ->
    Span = otter_lib_span:start(Name, TraceId, ParentId),
    put(otter_span_information, Span),
    Span.

%%----------------------------------------------------------------------
%% @doc start a new span with a list of tags on the process
%% dictionary of the current process and invoke the pre filter
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().
start_with_tags(Name, Tags) ->
    pre_filter(otter_lib_span:start_with_tags(Name, Tags)).

%%----------------------------------------------------------------------
%% @doc start a new span with a list of tags on the process
%% dictionary of the current process with an existing trace id or a parent
%% span.
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span();
                     (Name :: info(), Tags :: [tag()], ParentSpan :: span()) -> span().
start_with_tags(Name, Tags, #span{trace_id = TraceId, id = ParentId}) when is_integer(TraceId) ->
    pre_filter(otter_lib_span:start_with_tags(Name, Tags, TraceId, ParentId));
start_with_tags(Name, Tags, TraceId) when is_integer(TraceId) ->
    pre_filter(otter_lib_span:start_with_tags(Name, Tags, TraceId)).

%%----------------------------------------------------------------------
%% @doc start a new span with a list of tags on the process
%% dictionary of the current process with an existing trace_id and
%% parent_id
%% @end
%%----------------------------------------------------------------------
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id(), ParentId :: span_id()) -> span().
start_with_tags(Name, Tags, TraceId, ParentId) when is_integer(TraceId), is_integer(ParentId) ->
    pre_filter(otter_lib_span:start_with_tags(Name, Tags, TraceId, ParentId)).

%%----------------------------------------------------------------------
%% @doc Add a tag to the span in the process dictionary. If the span is
%% not active, a fake/empty span is returned with the default values and
%% timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec tag(Key :: info(), Value :: info()) -> span().
tag(Key, Value) ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:tag(Span, Key, Value),
            put(otter_span_information, NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.

%%----------------------------------------------------------------------
%% @doc Add a tag with specific service to the span in the process
%% dictionary. If the span is not active, a "fake" span is returned with
%% the default values and timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec tag(Key :: info(), Value :: info(), Service :: service()) -> span().
tag(Key, Value, Service) ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:tag(Span, Key, Value, Service),
            put(otter_span_information, NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.

%%----------------------------------------------------------------------
%% @doc Add a log to the span in the process dictionary. If the span is
%% not active, a "fake" span is returned with the default values and
%% timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec log(Text :: info()) -> span().
log(Text) ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:log(Span, Text),
            put(otter_span_information, NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.

%%----------------------------------------------------------------------
%% @doc Add a log with specific service to the span in the process
%% dictionary. If the span is not active, a "fake" span is returned with
%% the default values and timestamp set to 0.
%% @end
%%----------------------------------------------------------------------
-spec log(Text :: info(), Service :: service()) -> span().
log(Text, Service) ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            NewSpan = otter_lib_span:log(Span, Text, Service),
            put(otter_span_information, NewSpan),
            NewSpan;
        _ ->
            #span{timestamp = 0}
    end.

%%----------------------------------------------------------------------
%% @doc Finish collection of span information and invoke the span filter
%% unless the span is marked as inactive (timestamp set to 0)
%% @end
%%----------------------------------------------------------------------
-spec finish() -> ok.
finish() ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            otter_filter:span(otter_lib_span:finish(Span));
        _ ->
            ok
    end.

%%----------------------------------------------------------------------
%% @doc Return the trace id and span id of the span in the process
%% dictionary in a tuple. If there is no active span, return tuple {0, 0}
%% @end
%%----------------------------------------------------------------------
-spec ids() -> {trace_id(), span_id()}.
ids() ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            otter_lib_span:get_ids(Span);
        _ ->
            %% here we return the expected tuple to avoid crash when it
            %% is expected
            {0, 0}
    end.

%%----------------------------------------------------------------------
%% @doc Get the span from the process dictionary. If there is no active
%% span, return an empty one with timestamp set to 0
%% @end
%%----------------------------------------------------------------------
-spec get_span() -> span().
get_span() ->
    case get(otter_span_information) of
        Span when Span#span.timestamp =/= 0 ->
            Span;
        _ ->
            #span{timestamp = 0}
    end.

%%----------------------------------------------------------------------
%% @doc Put a span to the process dictionary. This can be used e.g. when
%% taking over a span started with the functional API.
%% @end
%%----------------------------------------------------------------------
-spec put_span(Span :: span()) -> term().
put_span(Span) ->
    put(otter_span_information, Span).


%%----------------------------------------------------------------------
%% Internal helper for filtering
%%----------------------------------------------------------------------
pre_filter(Span) ->
    case otter_filter:pre_span(Span) of
        #span{timestamp = 0} = InactiveSpan ->
            InactiveSpan;
        NewSpan ->
            put(otter_span_information, NewSpan),
            NewSpan
    end.
