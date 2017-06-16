[![Build Status](https://travis-ci.org/bluehouse-technology/otter.svg?branch=master)](https://travis-ci.org/Bluehouse-Technology/otter) [![Hex pm](http://img.shields.io/hexpm/v/otter.svg?style=flat)](https://hex.pm/packages/otter) [Docs](https://hexdocs.pm/otter)

# OTTER

OpenTracing Toolkit for ERlang

![otter logo](docs/images/otter_logo.png)

## Build

OTTER uses [rebar3](http://www.rebar3.org) as build tool.

```
    rebar3 compile
```

However most likely you'll want to add it to your project in your build
environment.

## Dependencies

- [otter_lib](https://github.com/Bluehouse-Technology/otter_lib) Common
library functions shared for otter and [otter_srv](https://github.com/Bluehouse-Technology/otter_srv)
- [otter_srv](https://github.com/Bluehouse-Technology/otter_srv) is a common test dependency (i.e. not part of production build)


In order to avoid external dependencies by default OTTER uses the OTP inets
HTTP client (httpc) to send spans to the trace collector. However httc has
a fairly bad reputation in high throughput scenarios. While IMO most of
these issues have been fixed in the past years, OTTER supports the following
HTTP clients as well :

- [ibrowse](https://github.com/cmullaparthi/ibrowse)
- [hackney](https://github.com/benoitc/hackney)

To use these clients they should be available (e.g. as application or release dependency) and
configured in OTTER (see configuration below).


## OpenTracing

[OpenTracing](http://opentracing.io) is an open initiative to provide a
set of terms and methods to produce, collect and correlate trace
information in a distributed environment across different programming
languages, platforms and protocols.

The concept defined for trace production is based on a **span** which is
essentially a record of a handling in one environment. A **span** has a
**timestamp** of when it started, a **duration**, a list of timestamped
events marking the timing of important actions during the **span** and a
list of key-value tags storing the parameters of the handled request
(e.g. customer ids, transaction ids, results of subsequent actions).
The **span** also contains id's to aid their correlation. The
**trace_id** is used for correlating **span**s related to the handling
of one request received from different systems. The **trace_id** is
generated in the first system which starts handling a request (e.g. a
frontend) and supposed to be passed on to other systems involved in the
processing the same request. This is fairly simple when the protocols
are fully under control and extensible (e.g. HTTP).
Other id's recorded are the **span_id** and a **parent_id** referring to
the parent **span** to help showing a hierarchical relationship of
the **span**s in the **trace collector**.

After collecting this information, the **span** can be sent to a trace
collector, which based on the id's of the received spans can
correlate them and provide and end-to-end view of the request.
Sending all produced **span**s could generate significant additional
load on the system that produces them and also on the **trace collector**.
It is recommended to filter the the **span**s before sending them to the
collector.

The most mature **trace collector** at the time of the initial development
 is [OpenZipkin](http://zipkin.io). OTTER provides an interface to send
spans to Zipkin using the HTTP/Thrift binary protocol. Since the
[Jaeger](https://uber.github.io/jaeger/) trace collector also supports the
Zipkin thrift protocol, it can also be used (thanks to [Yury Gargay](https://github.com/surik)
for this finding).

The OpenTracing terminology defines information to be passed on across
systems. The feasibility of this in most cases depends on the protocols
used, and sometimes rather difficult to achieve. OTTER is not attempting to
implement any of this functionality. It is possible though to initialize
a **span** in OTTER with a **trace_id** and **parent_id**, but how these
id's are passed across the systems is left to the particular implementation.

## OTTER OpenTracing compliance

While OTTER uses the basic concepts of OpenTracing, initially it is not
intended to be fully compliant. Some of the abstractions and separation of
functions/components to packages defined in OpenTracing didn't seem
practical to implement at this stage in Erlang. Other functions (e.g.
key-value logs, baggages, different type of span references, carrier span
inject/extract API) are not supported. Either because the trace collector
protocol OTTER initially supports (Zipkin thrift) does not support them, or
we have not seen a strong use case for these (yet).


## OTTER functionality

OTTER helps producing span information, filtering spans both when the span
is started (prefiltering) and when the span is completed, sending to
trace collector (Zipkin) and also counting/keeping a snapshot of the last
occurrence of a span.


![otter flow](docs/images/otter_flow.png)


### Producing span information

The main motivation behind the span collection of OTTER is to make the
instrumentation of existing code as simple as possible. OTTER includes 4
different APIs for this.

- Basic functional API

- Simple process dictionary API (for managing 1 span on the process dictionary)

- Multi span process dictionary API (for managing multiple spans with different names)

- Span ID API that manages a span in a separate process


#### Types used in the API

The following type specifications are in otter.hrl, which is part of the
otter_lib application.

```erlang
-type time_us() :: non_neg_integer().               % timestamp in microseconds
-type info()    :: binary() | iolist() | atom() | integer() | fun().
-type ip4()     :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type service() :: binary() | list() | default | {binary() | list(), ip4(), non_neg_integer()}.
-type trace_id():: non_neg_integer().
-type span_id() :: non_neg_integer().
-type action()  :: atom() | tuple().
-type tag()     :: {info(), info()} | {info(), info(), service()}.
-type log()     :: {time_us(), info()} | {time_us(), info(), service()}.

-record(span, {
          timestamp       :: time_us()  | undefined,% timestamp of starting the span
          trace_id        :: trace_id() | undefined,% 64 bit integer trace id
          name            :: info()     | undefined,% name of the span
          id              :: span_id()  | undefined,% 64 bit integer span id
          parent_id       :: span_id()  | undefined,% 64 bit integer parent span id
          tags = []       :: [tag()],               % span tags
          logs = []       :: [log()],               % span logs
          duration        :: time_us()  | undefined % microseconds between span start/end
         }).

-type span()    :: #span{}.
```

#### Functional API

The functional API is exposed in the ```otter``` module.

This API is provided as a basic method for manipulating span information.
Since it requires to pass the span data in function calls if the function has
something to add to the span. This requires more code changes, however
when functions pass non-strict composite structures (e.g. maps or
proplists) then inserting the span information is more or less trivial.
Very likely this is the least practical API for direct use, however it is
purely functional and could/should be used as a base for other APIs.

Start span with name only. Name should refer e.g. to the interface.

```erlang
-spec start(Name :: info()) -> span().
```

Start span with name and a parent span, the trace id and the parent span.
Trace id and parent id are taken from the parent span.

```erlang
-spec start(Name :: info(), ParentSpan :: span()) -> span().

```

Start span with name and trace id where trace id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> span().

```

Start span with name, trace id and parent span id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: integer()) -> span().

```

Start a span with name and initial tags. This function triggers pre-filtering
where the name and initial tags can be used to decide whether the span shall
be active or inactive (see later at filtering).

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().

```

Start a span with name, initial tags and a parent span. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], ParentSpan :: span()) -> span().

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span().

```

Start a span with name, initial tags and a trace id and a parent span id.
This function triggers pre-filtering.

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id(), ParentId :: span_id()) -> span().

```

Add a tag to the previously started span.

```erlang
-spec tag(Span :: span(), Key :: info(), Value :: info()) -> span().
```

Add a tag to the previously started span with additional service information.

```erlang
-spec tag(Span :: span(), Key :: info(), Value :: info(), Service :: service()) -> span().

```

Add a log/event to the previously started span

```erlang
-spec log(Span :: span(), Text :: info()) -> span().

```

Add a log/event to the previously started span with additional service information0

```erlang
-spec log(Span :: span(), Text :: info(), Service :: service()) -> span().

```

End span and invoke the span filter (see below)

```erlang
-spec finish(Span :: span()) -> ok.

```

Get span id's. Return the **trace_id** and the **span_id** from the
currently started span. This can be used e.g. when process "boundary" is
to be passed and eventually new span needs this information. Also when
these id's should be passed to a protocol interface for another system

```erlang
-spec ids(Span :: span()) -> {trace_id(), span_id()}.

```

example :

```erlang
    ...
    Span = otter:start("radius request"),
    ...
    ...
    Span1 = otter:tag(Span, "request_id", RequestId),
    ...
    ...
    Span2 = otter:log(Span1, "invoke user db"),
    ...
    ...
    Span3 = otter:log(Span2, "user db result"),
    Span4 = otter:tag(Span3, "user db result", "ok"),
    ...
    ...
    Span5 = otter:tag(Span4, "final result", "error"),
    Span6 = otter:tag(Span5, "final result reason", "unknown user"),
    otter:finish(Span6),
    ...
```

#### Simple process dictionary API

The simple process dictionary API is exposed in the ```otter_span_pdict_api```
 module.

This API uses the process dictionary to store span information.
Since it manages the span in the process dictionary of the current process,
it does not require to pass around any data, therefore this API is likely
the easiest to implement in existing request handler process code. The
limitations are that it is bound to a single process and that it can only
manage 1 span.

Start span with name only. Name should refer e.g. to the interface.

```erlang
-spec start(Name :: info()) -> span().
```

Start span with name and trace id where trace id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> span();

```

Start span with name and parent span. The trace id and parent id is taken
from the parent span.

```erlang
-spec start(Name :: info(), ParentSpan :: span()) -> span();

```

Start span with name and trace id.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> span();

```

Start span with name, trace_id and parent span id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: span_id()) -> span().

```

Start a span with name and initial tags. This function triggers pre-filtering
where the name and initial tags can be used to decide whether the span shall
be active or inactive (see later at filtering).

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().

```

Start a span with name, initial tags and a parent span. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], ParentSpan :: span()) -> span().

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span().

```

Start a span with name, initial tags and a trace id and a parent span id.
This function triggers pre-filtering.

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id(), ParentId :: span_id()) -> span().

```

Add a tag to the previously started span.

```erlang
-spec tag(Key :: info(), Value :: info()) -> span().

```

Add a tag to the previously started span with additional service information

```erlang
-spec tag(Key :: info(), Value :: info(), Service :: service()) -> span().

```

Add a log/event to the previously started span

```erlang
-spec log(Text :: info()) -> span().

```

Add a log/event to the previously started span with additional service information

```erlang
-spec log(Text :: info(), Service :: service()) -> span().

```


End span and invoke the span filter (see below)
```erlang
-spec finish() -> ok.

```

Get span id's. Return the **trace_id** and the **span_id** from the
currently started span. This can be used e.g. when process "boundary" is
to be passed and eventually new span needs this information. Also when
these id's should be passed to a protocol interface for another system

```erlang
-spec ids() -> {trace_id(), span_id()}.

```

Return the current span. e.g. it can be handed to another process to
continue collecting span information using the functional API.

```
-spec get_span() -> span().

```

example :

```erlang
    ...
    otter_span_pdict_api:start("radius request"),
    ...
    ...
    otter_span_pdict_api:tag("request_id", RequestId),
    ...
    ...
    otter_span_pdict_api:log("invoke user db"),
    ...
    ...
    otter_span_pdict_api:log("user db result"),
    otter_span_pdict_api:tag("user_db_result", "ok"),
    ...
    ...
    otter_span_pdict_api:tag("final_result", "error"),
    otter_span_pdict_api:tag("final_result_reason", "unknown user"),
    otter_span_pdict_api:finish(),
    ...
```

#### Multiple span process dictionary API

The process dictionary API with support of multiple spans in a process is
exposed in the ```otter_span_mpdict_api``` module.

This API works similarly to the simple process dictionary API, only it
stores the span information for each span name. i.e. it supports multiple
spans with different names in a process. Since the names are (normally)
static information, in ideal case this API also does not require extra
data/variable to be passed around.

Start span with name only. Name should refer e.g. to the interface.

```erlang
-spec start(Name :: info()) -> span().
```

Start span with name and trace id where trace id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> span();

```

Start span with name and parent span. The trace id and parent id is taken
from the parent span.

```erlang
-spec start(Name :: info(), ParentSpan :: span()) -> span();

```

Start span with name and trace id.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> span();

```

Start span with name, trace_id and parent span id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: span_id()) -> span().

```

Start a span with name and initial tags. This function triggers pre-filtering
where the name and initial tags can be used to decide whether the span shall
be active or inactive (see later at filtering).

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> span().

```

Start a span with name, initial tags and a parent span. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], ParentSpan :: span()) -> span().

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span().

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> span().

```

Add a tag to the previously started span.

```erlang
-spec tag(Name :: info(), Key :: info(), Value :: info()) -> span().

```

Add a tag to the previously started span with additional service information

```erlang
-spec tag(Name :: info(), Key :: info(), Value :: info(), Service :: service()) -> span().

```

Add a log/event to the previously started span

```erlang
-spec log(Name :: info(), Text :: info()) -> span().

```

Add a log/event to the previously started span with additional service information

```erlang
-spec log(Name :: info(), Text :: info(), Service :: service()) -> span().

```


End span and invoke the span filter (see below)
```erlang
-spec finish(Name :: info()) -> ok.

```

Get span id's. Return the **trace_id** and the **span_id** from the
currently started span. This can be used e.g. when process "boundary" is
to be passed and eventually a new span needs this information. Also when
these id's should be passed to a protocol interface for another system

```erlang
-spec ids(Name :: info()) -> {trace_id(), span_id()}.

```

Return the current span. e.g. it can be handed to another process to
continue collecting span information using the functional API.

```
-spec get_span(Name :: info()) -> span().

```

example :

```erlang
    ...
    otter_span_mpdict_api:start("radius request"),
    ...
    ...
    otter_span_mpdict_api:tag("radius request", "request_id", RequestId),
    ...
    ...
    otter_span_mpdict_api:log("radius request", "invoke user db"),
    ...
    ...
    otter_span_mpdict_api:log("radius request", "user db result"),
    otter_span_mpdict_api:tag("radius request", "user_db_result", "ok"),
    ...
    ...
    otter_span_mpdict_api:tag("radius request", "final_result", "error"),
    otter_span_mpdict_api:tag("radius request", "final_result_reason", "unknown user"),
    otter_span_mpdict_api:finish("radius request"),
    ...
```
#### Span id API

The Span id API is exposed from the ```otter_span_id_api``` module.

This API spawns a separate process for each started span and uses the PID
of this collector process to identify the span. API calls send messages to
this process. If there are no messages (tag, log, finish) received for a
time period configured in ```span_id_api_process_timeout``` otter application
configuration parameter (in milliseconds, default 30000) then the process
exits, all eventual subsequent span actions are ignored and the span is
discarded. Advantage of this API could be that the PID does not change as
it would with the functional API and it is not bound to a particular request
process as the process dictionary APIs. Disadvantage is the increased
resource consumption, especially the potentially large amount of additional
processes spawned on a fairly busy system. Span pre-filtering could be used
to help this.

Start span with name only. Name should refer e.g. to the interface.

```erlang
-spec start(Name :: info()) -> pid().
```

Start span with name and trace id where trace id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> pid();

```

Start span with name and parent span. The trace id and parent id is taken
from the parent span.

```erlang
-spec start(Name :: info(), ParentSpan :: span()) -> pid();

```

Start span with name and trace id.

```erlang
-spec start(Name :: info(), TraceId :: trace_id()) -> pid();

```

Start span with name, trace_id and parent span id e.g. received from
protocol.

```erlang
-spec start(Name :: info(), TraceId :: trace_id(), ParentId :: span_id()) -> pid().

```

Start a span with name and initial tags. This function triggers pre-filtering
where the name and initial tags can be used to decide whether the span shall
be active or inactive (see later at filtering).

```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()]) -> pid() | undefined.

```

Start a span with name, initial tags and a parent span. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], ParentSpan :: span()) -> pid() | undefined.

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> pid() | undefined.

```

Start a span with name, initial tags and a trace id. This function triggers
pre-filtering.


```erlang
-spec start_with_tags(Name :: info(), Tags :: [tag()], TraceId :: trace_id()) -> pid() | undefined.

```

Add a tag to the previously started span.

```erlang
-spec tag(Pid :: pid() | undefined, Key :: info(), Value :: info()) -> ok.

```

Add a tag to the previously started span with additional service information

```erlang
-spec tag(Pid :: pid() | undefined, Key :: info(), Value :: info(), Service :: service()) -> ok.

```

Add a log/event to the previously started span

```erlang
-spec log(Pid :: pid() | undefined, Text :: info()) -> ok.

```

Add a log/event to the previously started span with additional service information

```erlang
-spec log(Pid :: pid() | undefined, Text :: info(), Service :: service()) -> ok.

```


End span and invoke the span filter (see below). If the span is inactive,
the span is discarded.


```erlang
-spec finish(Pid :: pid() | undefined) -> ok.

```

Get span id's. Return the **trace_id** and the **span_id** from the span.
In case the span is inactive i.e. the Pid is undefined, this function
returns the tuple ```{0, 0}```.

```erlang
-spec ids(Pid :: pid() | undefined) -> {trace_id(), span_id()}.

```


example :

```erlang
    ...
    SpanPid = otter_span_id_api:start("radius request"),
    ...
    ...
    otter_span_id_api:tag(SpanPid, "request_id", RequestId),
    ...
    ...
    otter_span_id_api:log(SpanPid, "invoke user db"),
    ...
    ...
    otter_span_id_api:log(SpanPid, "user db result"),
    otter_span_id_api:tag(SpanPid, "user_db_result", "ok"),
    ...
    ...
    otter_span_id_api:tag(SpanPid, "final_result", "error"),
    otter_span_id_api:tag(SpanPid, "final_result_reason", "unknown user"),
    otter_span_id_api:finish(SpanPid),
    ...
```

#### tag/log information

A note on the tag key/value and log types: the Zipkin interface requires
string types. The Zipkin connector module (otter_conn_zipkin) attempts
to convert: integer, atom, and iolist types to binary. Unknown data types
(e.g. record, tuples, or maps) are converted using the "~p" io:fwrite formating
control character. The resulting string might be hard to read for non-Erlang
people, but it is still better than loosing the information completely.

If the generation of log values is complex or computational expensive, a
arity zero fun can be passed as info. The function is executed in the
connector module and thereby after span_end has been called.

Adding service information to tags and logs means that otter adds a host
structure to each of these elements. The extra optional service parameter
in the relevant API calls can have 3 formats.

The atom ```default``` will include service/host information based on
the following configuration parameters of the zipkin connector.

```erlang
    ...
    {zipkin_tag_host_ip, {127,0,0,1}},
    {zipkin_tag_host_port, 0},
    {zipkin_tag_host_service, "otter_test"},
    ...
```

Name of the service as a string in binary() or list() format also adds
the host information based on the configuration above, except the service
name will be as specified in the parameter.

A 3 element tuple ```{Service, Ip, Port}``` which will be used to compose
the information. This can be interesting to compose "ca" and "sa" opentracing
tags where the host information may refer to a remote server/client node
instead of the one where the span is generated.

#### Configuration

There is no configuration involved in the stage of producing span data.
The paramers mentioned above are functionally specific to the zipkin
connector. It was simpler to explain them though in this context.

### Span Filtering

#### Pre-filtering

When a span is started with the ```start_with_tags``` function, it triggers
pre-filtering with the otter_span_name and the initial tags provided in the
function call.

The filter is defined with the ```prefilter_rules``` otter application
configuration and has the same syntax as normal filter rules at span
completion. The same conditions can be used, however the actions are different.

Prefilter actions can be : ```allow, discard, {snapshot_count, [...], [...]}```

The logic of the prefilter is also different, as it stops at the first
matching condition set and executes the actions found there. In final span
filter, all rules are checked and all matching actions are collected/executed.

When the prefilter has the action ```discard``` in the matched actions, the
behaviour is dependent on the API.

With the basic functional API, the span timestamp is set to 0 indicating
that the span is inactive. Any subsequent action on an inactive span is
ignored and the final filter is not triggered (i.e. the span is discarded).
If ids are requested for an inactive span, the tuple ```{0,0}``` is returned.
```otter.hrl``` in the ```otter_lib``` application defines the ```?is_span_active(Span)```
macro that can be used in guards if necessary.

With both process dictionary APIs the behaviour is similar to the functional API.

The span id API does not start a span collection process for inactive spans,
and returns ```undefined``` instead of the Pid. All subsequent span actions
accept ```undefined``` Pid and in that case the action is ignored.

#### Final filtering

When the collection of **span** information is completed (i.e. span finish
is called), the filtering is invoked. Filtering is based on the
tags collected in the span with the **span name** and the
**span duration** added to the key/value pair list with keys :
**otter_span_name** and **otter_span_duration**. The resulting
key/value pair list which is used as input of the filter rules. With the
 examples above it can look like this :

```erlang
    [
        {otter_span_name, "radius request"},
        {otter_span_duration, 1202},
        {"request_id", "6390266399200312"},
        {"user_db_result", "ok"},
        {"final_result", "error"},
        {"final_result_reason", "unknown user"}
    ]
```

This key/value pair list is passed to a sequence of conditions/actions
pairs. In each pair the, conditions are a list of checks against the
key/value pair list. If all conditions in the list are true, the actions
are executed. An empty condition list always returns a positive match.

#### Filter conditions

##### Check the presence of a Key


```erlang
    {present, Key}
```

##### Check whether 2 Keys have the same value

```erlang
    {same, Key1, Key2}
```

##### Compare a value

The value of a Key/Value pair can be compared to a value

```erlang
    {value, Key, ValueToCompare}
```

example: check the name of the span

```erlang
    {value, otter_span_name, "radius request"}
```

##### Checking integer values

Key/Value pairs with integer values can be checked with the following
conditions.

```erlang
    {greater, Key, Integer}

    {less, Key, Integer}

    {between, Key, Integer1, Integer2}
```

example: check whether the span duration is greater than 5 seconds

```erlang
    {greater, otter_span_duration, 5000000}
```

##### Negate condition check

```erlang
    {negate, Condition}
```

example: Check if the final result is other than ok

```erlang
    {negate, {value, "final_result", "ok"}}
```

##### One out of

This condition uses a random generated number and in the range of 0 < X =< Integer,
then checks for the value 1. This filter is typically meant for pre-filtering.

```erlang
    {one_out_of, Integer}
```

example: Match 1 out of 1000 requests

```erlang
    {one_out_of, 1000}
```


#### Filter Actions

##### Snapshot/Count

Snapshot/Count increases a counter with a key composed by a fixed prefix and
values of Key/Values in the Key/Value list. The key is a list of parameters.
Also it stores the last **span** that triggers the counter in an ets
table. These cheap snapshots can be used for initial analysis of eventual
problems. The snapshots and counter can be retrieved by the otter counter
API (see below).

```erlang
    {snapshot_count, Prefix, KeyList}
```

example: snapshot/count any request that take long in different counters for
each span name and final result. The condition example above with the
otter_span_duration could be used to trigger this action.

```erlang
    {snapshot_count, [long_request], [otter_span_name, "final_result"]}
```

This will produce a counter and snapshot with e.g. such key :

```erlang
    [long_request, "radius request", "ok"]
```

##### Send span to Zipkin

This action triggers sending the span to Zipkin. This action can only be
used in final filter rules.

```erlang
    send_to_zipkin
```

##### Allow or discard in prefilter

Pre-filter actions can be (next to snapshot_count) the atoms ```allow```
or ```discard```. When a span is discarded, it is marked as inactive and
the different APIs do their best to ignore it with as little resource consumption
as possible, while keeping the API contract (i.e. same instrumentation works
with active and inactive spans)


#### Filter configuration

The final filter rules are configured under **filter_rules** and the prefilter
rules in **prefilter_rules**. If filter rules are not defined, the spans are
discarded, if prefilter rules are not defined, all spans handled as active.

##### Example

example Condition/Action (rule) list:

```erlang
    [
        {
            %% Condition
            [
                {greater, "otter_span_duration", 5000000},
                {value, "otter_span_name", "radius request"}
            ],
            %% Action
            [
                {snap_count, [long_radius_request], []},
                send_to_zipkin
            ]
        },
        {
            %% Condition counts all requests with name and result
            [
            ],
            %% Action
            [
                {snap_count, [request], ["otter_span_name", "final_result"]}
            ]
        }

    ]
```

##### External filter functions

Both **filter_rules** and **prefilter_rules** configurations can have a
```{Module, Function, ExtraArgument}``` tuple as value. With this configuration
```Module:Function(Span :: span(), ExtraArgument)``` will be called and
expected to return a tuple ```{NewSpan :: span(), Actions :: [action()]}```.
Any other results are ignored and the span is discarded. The external filter
can also modify the span. The actions are as described above. Unknown actions
in the list are ignored. Of course the external filter can do any additional
actions before returning the result.


### Sending a span to Zipkin

As a result of filter action **send_to_zipkin** the span is forwarded to
the trace collector using HTTP/Thrift binary protocol. In the context of
the span producing process the span is added to a buffer (ETS table). The
content of this buffer is sent to Zipkin asynchronously in intervals
configured in **zipkin_batch_interval_ms** (milliseconds).

The URI of the Zipkin trace collector is configured in **zipkin_collector_uri**.
When [Jaeger](https://uber.github.io/jaeger/) is used then the URL should
also include **"format=zipkin.thrift"** request parameter. e.g.

```erlang
    ...
    {zipkin_collector_uri, "http://127.0.0.1:14268/api/traces?format=zipkin.thrift"},
    ...
```

Tags ane logs in Zipkin can have an optional node entry. This entry contains
the service name and IP/Port of the node sending the span. The Zipkin connector
module (otter_conn_zipkin.erl) can add an extra tag to each span during
encoding the span by setting the **zipkin_add_host_tag_to_span** in the
configuration. The value of the parameter should  a tuple ```{Key, Value}```.
OpenZipkin uses the "lc" (Local Component) tag to display the service for a span.

example :

```erlang
    {zipkin_add_host_tag_to_span, {"lc", ""}},
```

The default service/host information to be sent to zipkin is provided in
**zipkin_tag_host_service**, **zipkin_tag_host_ip** and **zipkin_tag_host_port**
configuration parameters.

Sending the span to Zipkin utilizes the configured HTTP client.
OTTER has support for inets https, ibrowse or hackney. Alternatively a
callback function can be configured to plug in other http clients.

The configuration to specify the http client is **http_client** which can
have the vaue of the atoms ```httpc``` (default), ```ibrowse```, ```hackney```
or the tuple ```{Module, Function}```. Except for httpc the other clients
should be added to the project dependencies. OTTER does not include them.


In case ```{Module, Function}``` is configured then ```Module:Function(ZipkinUrl :: list(), BinaryThriftData :: binary())```
is called. The callback is expected to return ```{ok, HTTPResultCode :: integer()}```.
Normally callback should send a HTTP POST request to OpenZipkin or Jaeger setting
the content type to **application/x-thrift**. Both of these trace collectors
return HTTP result code 202 in success case.



### Snapshot/Counter

As a result of the filter snap_count action, 2 ETS tables are used to
count events (see snap_count action above) and in the same time store
the last span information that has increased the counter. This can be
considered a useful and fairly cheap troubleshooting tool.

Tho retrieve and manage these snapshot counters, OTTER provides API calls
 in the otter API module.

##### List counters

```erlang
    -spec counter_list() -> [{list(), integer()}].
```

example:

```erlang
3> otter:counter_list().
[{[long_span,test_request],1},
 {[otter_conn_zipkin,send_spans,failed],1},
 {[span_processed,"customer db lookup"],1},
 {[span_processed,test_request],1},
 {[long_span,"customer db lookup"],1}]
```

##### Retrieve snapshot for counter

```erlang
    -spec counter_snapshot(list()) -> term().
```

example:

```erlang
4> otter:counter_snapshot([long_span,test_request]).
[{[long_span,test_request],
  [{snap_timestamp,{2017,2,21,19,8,23,76525}},
   {data,{span,1487700503067982,3826404163842487863,
               test_request,1113017739039451686,undefined,
               [{customer_id,1},
                {transaction_id,3232323},
                {magic_tag,"wizz"},
                {magic_result,error},
                {db_result,"bad customer"},
                {final_result,error}],
               [{1487700503067998,"starting some magic"},
                {1487700503068000,"finished magic"},
                {1487700503068012,"db lookup"},
                {1487700503076491,"db lookup returned"}],
               8525}}]}]
```

##### Delete counter (and its snapshot)

```erlang
    -spec counter_delete(list()) -> ok.
```

##### Delete all counters and snapshots

```erlang
    -spec counter_delete_all() -> ok.
```

## OTTER Configuration

The OTTER application configuration is handled through the otter_config
module (otter_config.erl). In the default implementation it uses the
application environment. An example configuration can be found in the
otter.app.src file.

```erlang
    ...
    {http_client, ibrowse}, %% ibrowse | httpc
    {zipkin_collector_uri, "http://172.17.0.2:9411/api/v1/spans"},
    {zipkin_batch_interval_ms, 100},
    {zipkin_tag_host_ip, {127,0,0,1}},
    {zipkin_tag_host_port, 0},
    {zipkin_tag_host_service, "otter_test"},
    {zipkin_add_host_tag_to_span, {"lc", ""}},
    {filter_rules, [
        {
            [
                {greater, otter_span_duration, 1000}
            ],
            [
                {snapshot_count, [long_span], [otter_span_name]}
            ]
        },
        {
            [],
            [
                {snapshot_count, [span_processed], [otter_span_name]},
                send_to_zipkin
            ]
        }
    ]},
    ...
```

## Acknowledgements

The development of ''otter'' was championed by [Holger Winkelmann](https://github.com/hwinkel) of [Travelping](https://github.com/travelping). Both [Travelping](https://github.com/travelping) and [bet365](http://bet365.com) kindly provided sponsorship for the initial development. The development was primarily done by [Ferenc Holzhauser](https://github.com/fholzhauser) with design input from [Chandru Mullaparthi](https://github.com/cmullaparthi).

## License

Apache 2.0
