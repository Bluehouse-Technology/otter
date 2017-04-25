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
%%% @doc This module supports both the functional and p-dict style
%%% APIs. This is for internal use. Use the API in the `otter' module
%%% primarily, or if you want to use the process dictionary API, use
%%% the `otter_span_pdict_api' module 
%%% @end
%%% -------------------------------------------------------------------

-module(otter_span).
-export([
         fstart/1, fstart/2, fstart/3,
         fend/1,
         fget_ids/1,
         flog/2, flog/3,
         ftag/3, ftag/4,

         pstart/1, pstart/2, pstart/3,
         pend/0,
         pget_span/0,
         pget_ids/0,
         plog/1, plog/2,
         ptag/2, ptag/3
        ]).

-include("otter.hrl").

fstart(Name) ->
    fstart(Name, otter_lib:id()).
fstart(Name, TraceId) ->
    fstart(Name, TraceId, undefined).
fstart(Name, TraceId, ParentId) ->
    #span{
        timestamp = otter_lib:timestamp(),
        trace_id = TraceId,
        id = otter_lib:id(),
        parent_id = ParentId,
        name = Name
    }.

ftag(Span, Key, Value) ->
    Tags = Span#span.tags,
    Span#span{
        tags = lists:keystore(Key, 1, Tags, {Key, Value})
    }.

ftag(Span, Key, Value, Service) ->
    Tags = Span#span.tags,
    Span#span{
        tags = lists:keystore(Key, 1, Tags, {Key, Value, Service})
    }.

flog(Span, Text) ->
    Logs = Span#span.logs,
    Span#span{
        logs = [{otter_lib:timestamp(), Text} | Logs]
    }.

flog(Span, Text, Service) ->
    Logs = Span#span.logs,
    Span#span{
        logs = [{otter_lib:timestamp(), Text, Service} | Logs]
    }.

fend(Span) ->
    Start = Span#span.timestamp,
    Logs = Span#span.logs,
    otter_filter:span(Span#span{
        duration = otter_lib:timestamp() - Start,
        logs = lists:reverse(Logs)
    }),
    ok.

fget_ids(Span) ->
    #span{trace_id = TraceId, id = Id} = Span,
    {TraceId, Id}.

%% ====================  SPAN process API  ======================
%% This API uses the process dictionary to collect span information
%% and can be used when all span tags an events happen in the same
%% request handling process.

pstart(Name) ->
    pstart(Name, otter_lib:id()).

pstart(Name, TraceId) ->
    pstart(Name, TraceId, undefined).

pstart(Name, TraceId, ParentId) ->
    Span = fstart(Name, TraceId, ParentId),
    put(otter_span_information, Span),
    Span.

ptag(Key, Value) ->
    Span = ftag(get(otter_span_information), Key, Value), 
    put(otter_span_information, Span),
    Span.

ptag(Key, Value, Service) ->
    Span = ftag(get(otter_span_information), Key, Value, Service),
    put(otter_span_information, Span),
    Span.

plog(Text) ->
    Span = flog(get(otter_span_information), Text),
    put(otter_span_information, Span),
    Span.

plog(Text, Service) ->
    Span = flog(get(otter_span_information), Text, Service),
    put(otter_span_information, Span),
    Span.

pend() ->
    Span = get(otter_span_information),
    fend(Span).

%% This call can be used to retrieve the IDs from the calling process
%% e.g. when you have a gen_server and you get an API function call
%% (which is in the context of the calling process) then calling pget_id()
%% returns a {TraceId, SpanId} that is stored with the process API calls
%% above for the calling process, so they can be used in the handling of
%% the call
pget_ids() ->
    #span{trace_id = TraceId, id = Id} = get(otter_span_information),
    {TraceId, Id}.

pget_span() ->
    get(otter_span_information).
