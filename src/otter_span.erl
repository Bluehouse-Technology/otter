-module(otter_span).
-compile(export_all).
-include("otter.hrl").

%% ====================  SPAN function API  ======================
%% This API functions with passing around the Span in the function calls

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
    put(otter_span_information, fstart(Name, TraceId, ParentId)),
    ok.

ptag(Key, Value) ->
    Span = get(otter_span_information),
    put(otter_span_information, ftag(Span, Key, Value)),
    ok.

ptag(Key, Value, Service) ->
    Span = get(otter_span_information),
    put(otter_span_information, ftag(Span, Key, Value, Service)),
    ok.

plog(Text) ->
    Span = get(otter_span_information),
    put(otter_span_information, flog(Span, Text)),
    ok.

plog(Text, Service) ->
    Span = get(otter_span_information),
    put(otter_span_information, flog(Span, Text, Service)),
    ok.

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
