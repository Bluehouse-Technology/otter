-module(otter_span).
-compile(export_all).


%% ====================  SPAN process API  ======================
%% This API uses the process dictionary to collect span information
%% and can be used when all span tags an events happen in the same
%% request handling process.

pstart(Name) ->
    pstart(Name, otter_lib:id()).

pstart(Name, TraceId) ->
    pstart(Name, TraceId, undefined).

pstart(Name, TraceId, ParentId) ->
    put(
        otter_span_information, #{
        timestamp => otter_lib:timestamp(),
        trace_id => TraceId,
        id => otter_lib:id(),
        parent_id => ParentId,
        name => Name
    }),
    ok.

ptag(Key, Value) ->
    Span = get(otter_span_information),
    Tags = maps:get(tags, Span, []),
    put(otter_span_information, Span#{
        tags => lists:keystore(Key, 1, Tags, {Key, Value})
    }),
    ok.

plog(Text) ->
    Span = get(otter_span_information),
    Logs = maps:get(logs, Span, []),
    put(otter_span_information, Span#{
        logs => Logs ++ [{otter_lib:timestamp(), Text}]
    }),
    ok.

pend() ->
    Span = get(otter_span_information),
    erase(otter_span_information),
    Start = maps:get(timestamp, Span),
    otter_filter:span(Span#{
        duration => otter_lib:timestamp()-Start
    }),
    ok.

%% This call can be used to retrieve the IDs from the calling process
%% e.g. when you have a gen_server and you get an API function call
%% (which is in the context of the calling process) then calling pget_id()
%% returns a {TraceId, SpanId} that is stored with the process API calls
%% above for the calling process, so they can be used in the handling of
%% the call
pget_ids() ->
    #{trace_id := TraceId, id := Id} = get(otter_span_information),
    {TraceId, Id}.

pget_span() ->
    get(otter_span_information).

%% ====================  SPAN function API  ======================
%% This API functions with passing around the Span in the function calls

fstart(Name) ->
    fstart(Name, otter_lib:id()).
fstart(Name, TraceId) ->
    fstart(Name, TraceId, undefined).
fstart(Name, TraceId, ParentId) ->
    #{
        timestamp => otter_lib:timestamp(),
        trace_id => TraceId,
        id => otter_lib:id(),
        parent_id => ParentId,
        name => Name
    }.

ftag(Span, Key, Value) ->
    Tags = maps:get(tags, Span, []),
    Span#{
        tags => lists:keystore(Key, 1, Tags, {Key, Value})
    }.

flog(Span, Text) ->
    Logs = maps:get(logs, Span, []),
    Span#{
        logs => Logs ++ [{otter_lib:timestamp(), Text}]
    }.

fend(Span) ->
    Start = maps:get(timestamp, Span),
    otter_filter:span(Span#{
        duration => otter_lib:timestamp()-Start
    }),
    ok.

fget_ids(Span) ->
    #{trace_id := TraceId, id := Id} = Span,
    {TraceId, Id}.
