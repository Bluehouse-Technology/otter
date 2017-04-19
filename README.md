[![Build Status](https://travis-ci.org/bluehouse-technology/otter.svg)](https://travis-ci.org/Bluehouse-Technology/otter) [![Hex pm](http://img.shields.io/hexpm/v/otter.svg?style=flat)](https://hex.pm/packages/otter) [Docs](https://hexdocs.pm/otter)

# OTTER

OpenTracing Toolkit for ERlang

![otter logo](docs/images/otter_logo.png)

## Build

OTTER uses [rebar3](http://www.rebar3.org) as build tool. It can be built
with:

```
    rebar3 compile
```

However most likely you'll want to add it to your project in your build
environment.

## Dependencies

[ibrowse](https://github.com/cmullaparthi/ibrowse) HTTP client is used
to send the HTTP/Thrift requests to Zipkin.

[cowboy](https://github.com/ninenines/cowboy) HTTP server (version 1.x API)
is used to receive HTTP/Thrift (Zipkin binary format).

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
spans to Zipkin using the HTTP/Thrift binary protocol.

The OpenTracing terminology defines information to be passed on across
systems. The feasibility of this in most cases depends on the protocols
used, and sometimes rather difficult to achieve. OTTER is not attempting to
implement any of this functionality. It is possible though to initialize
a **span** in OTTER with a **trace_id** and **parent_id**, but how these
id's are passed across the systems is left to the particular implementation.

## OTTER functionality

OTTER helps producing span information, filtering spans, sending to
trace collector (Zipkin) and also counting and keeping a snapshot of the last
occurrence of a span.


![otter flow](docs/images/otter_flow.png)


### Producing span information

The main motivation behind the span collection of Otter is to make the
instrumentation of existing code as simple as possible.

#### Types used in the API

The following type specifications are in otter.hrl

```erlang
-type time_us() :: integer().           % timestamp in microseconds
-type info()    :: binary() | iolist() | atom() | integer().
-type ip4()     :: {integer(), integer(), integer(), integer()}.
-type service() :: binary() | list() | default | {binary() | list(), ip4(), integer()}.
-type trace_id():: integer().
-type span_id() :: integer().

-record(span , {
    timestamp   :: time_us(),           % timestamp of starting the span
    trace_id    :: trace_id(),          % 64 bit integer trace id
    name        :: info(),              % name of the span
    id          :: span_id(),           % 64 bit integer span id
    parent_id   :: span_id() | undefined, % 64 bit integer parent span id
    tags = []   :: [{info(), info()} | {info(), info(), service()}],  % span tags
    logs = []   :: [{time_us(), info()} | {info(), info(), service()}], % span logs
    duration    :: time_us()            % microseconds between span start/end
}).

-type span()    :: #span{}.

```

#### Functional API

Passing the span structure between requests, this API will probably
require to pass the span in function calls if the function has
something to add to the span. This requires more code changes, however
when functions pass non-strict composite structures (e.g. maps or
proplists) then inserting the span information is more or less trivial.

Start span with name only. Name should refer e.g. to the interface.
```erlang
-spec span_start(Name::info()) -> span().
```

Start span with name and trace_id where trace_id e.g. received from
protocol.

```erlang
-spec span_start(Name::info(), TraceId::integer()) -> span().
```

Start span with name, trace_id and parent span id e.g. received from
protocol.

```erlang
-spec span_start(Name::info(), TraceId::integer(), ParentId::integer()) -> span().
```

Add a tag to the previously started span.
```erlang
-spec span_tag(Span::span(), Key::info(), Value::info()) -> span().
```

Add a tag to the previously started span with additional service information.
```erlang
-spec span_tag(Span::span(), Key::info(), Value::info(), Service::service()) -> span().
```

Add a log/event to the previously started span
```erlang
-spec span_log(Span::span(), Text::info()) -> span().
```

Add a log/event to the previously started span with additional service information0
```erlang
-spec span_log(Span::span(), Text::info(), Service::service()) -> span().
```

End span and invoke the span filter (see below)
```erlang
-spec span_end(Span::span()) -> ok.
```

Get span id's. Return the **trace_id** and the **span** id from the
currently started span. This can be used e.g. when process "boundary" is
to be passed and eventually new span needs this information. Also when
these id's should be passed to a protocol interface for another system
```erlang
-spec span_ids(span()) -> {trace_id(), span_id()}.
```
example :

```erlang
    ...
    Span = otter:span_start("radius request"),
    ...
    ...
    Span1 = otter:span_tag(Span, "request_id", RequestId),
    ...
    ...
    Span2 = otter:span_plog(Span1, "invoke user db"),
    ...
    ...
    Span3 = otter:span_plog(Span2, "user db result"),
    Span4 = otter:span_ptag(Span3, "user db result", "ok"),
    ...
    ...
    Span5 = otter:span_ptag(Span4, "final result", "error"),
    Span6 = otter:span_ptag(Span5, "final result reason", "unknown user"),
    otter:span_pend(Span6),
    ...
```

#### Process API

The simplest API uses the process dictionary to store span information.
This is probably the least work to implement in existing code.

Start span with name only. Name should refer e.g. to the interface.

```erlang
-spec span_pstart(Name::info()) -> ok.
```

Start span with name and trace_id where trace_id e.g. received from
protocol.

```erlang
-spec span_pstart(Name::info(), TraceId::trace_id()) -> ok.
```

Start span with name, trace_id and parent span id e.g. received from
protocol.

```erlang
-spec span_pstart(Name::info(), TraceId::trace_id(), ParnetId::span_id()) -> ok.
```

Add a tag to the previously started span.

```erlang
-spec span_ptag(Key::info(), Value::info()) -> ok.
```

Add a tag to the previously started span with additional service information

```erlang
-spec span_ptag(Key::info(), Value::info(), Service::service()) -> ok.
```

Add a log/event to the previously started span
```erlang
-spec span_plog(Text::info()) -> ok.
```

Add a log/event to the previously started span with additional service information
```erlang
-spec span_plog(Text::info(), Service::service()) -> ok.
```


End span and invoke the span filter (see below)
```erlang
-spec span_pend() -> ok.
```

Get span id's. Return the **trace_id** and the **span** id from the
currently started span. This can be used e.g. when process "boundary" is
to be passed and eventually new span needs this information. Also when
these id's should be passed to a protocol interface for another system

```erlang
-spec span_pids() -> {trace_id(), span_id()}.
```

Return the current span. e.g. it can be handed to another process to
continue collecting span information using the functional API.

```
-spec span_pget() -> span().
```

example :

```erlang
    ...
    otter:span_pstart("radius request"),
    ...
    ...
    otter:span_ptag("request_id", RequestId),
    ...
    ...
    otter:span_plog("invoke user db"),
    ...
    ...
    otter:span_plog("user db result"),
    otter:span_ptag("user_db_result", "ok"),
    ...
    ...
    otter:span_ptag("final_result", "error"),
    otter:span_ptag("final_result_reason", "unknown user"),
    otter:span_pend(),
    ...
```

#### tag/log information

A note on the tag key/value and log types: the Zipkin interface requires
string types. The Zipkin connector module (otter_conn_zipkin.erl) attempts
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

When the collection of **span** information is completed (i.e. span_pend
or span_end/1 is called), filtering is invoked. Filtering is based on the
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
    {greater, "otter_span_duration", 5000000}
```

##### Negate condition check

```erlang
    {negate, Condition}
```

example: Check if the final result is other than ok

```erlang
    {negate, {value, "final_result", "ok"}}
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

This action triggers sending the span to Zipkin

```erlang
    send_to_zipkin
```

##### Stop evaluating further Condition/Action pairs

Normally each span triggers checking of all Condition/Action pairs in the
sequence (executing all relevant actions). However if for a particular set
of conditions it is not necessary, the break action can be used. When
this is found in an Action list then all actions in the current list are
executed and no further Condition/Action pairs are checked.

```erlang
    break
```

#### Filter configuration

The filter rules are configured under **filter_rules**

#### Example

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

### Sending a span to Zipkin

As a result of filter action **send_to_zipkin** the span is forwarded to
the trace collector using HTTP/Thrift binary protocol. In the context of
the span producing process the span is added to a buffer (ETS table). The
content of this buffer is sent to Zipkin asynchronously in intervals
configured in **zipkin_batch_interval_ms** (milliseconds).

The URI of the Zipkin trace collector is configured in **zipkin_collector_uri**.

Zipkin requires a node entry for every single tag collected in the span.
This entry contains the service name and IP/Port of the node sending the
span. If there is no tag/log is produced during span collecion with service/host
paramer, the Zipkin connector module (otter_conn_zipkin.erl) can be configured
to add an extra tag to each span during encoding the span by setting the
**zipkin_add_host_tag_to_span** in the configuration. The value of the
parameter should  a tuple ```{Key, Value}``` which will be used as the
default tag information. OpenZipkin uses the "lc" (Local Component) tag
to display the service for a span.

example :

```erlang
    {zipkin_add_host_tag_to_span, {"lc", ""}},
```

The default service/host information to be sent to zipkin is provided in
**zipkin_tag_host_service**, **zipkin_tag_host_ip** and **zipkin_tag_host_port**
configuration parameters.

It is also possible to add the default service/host information (as specified)
in the configuration paramers above to each tag/log that does not have it
explicitly set by setting the **zipkin_add_default_service_to_logs** and
**zipkin_add_default_service_to_tags** to ```true```. This however can increase
the amount of data to be sent to zipkin significantly (depending on the
number of logs/tags) so probably it is not recommended for most uses.

example :

```erlang
    ...
    {zipkin_add_default_service_to_logs, false},
    {zipkin_add_default_service_to_tags, false},
    ...
```

Sending the span to Zipkin utilizes the [ibrowse](https://github.com/cmullaparthi/ibrowse)
http client (which is the only dependency of OTTER).

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

### Built in HTTP/Thrift (zipkin protocol) server

OTTER has a HTTP/Thrift server that understands the Zipkin binary protocol.
Originally was intended for testing purposes, but probably it can be generally
useful to write own trace collector, or custom correlator for instrumented
services.

The server is started up during otter application start if the
**server_zipkin_callback** configuration parameter is set to a ```{Module, Function}```
tuple. The listening port can be specified with **server_zipkin_port** and
defaults to 9411 if not defined. When spans are received on the server,
they are decoded to otter ```#span{}``` record and individually handed to
the specified callback. There is a stub/example callback module (otter_server_span_cb.erl)
in the application. All the string types in the span are decoded to ```binary()```
types. i.e. it is not completely symmetrical with the encoder functions. While the
encoder is "forgiving" when it comes to log text, tag key/values and other
string parameters by accepting ```atom() | binary() | list() | integer()```
for convenience, obviously the decoder can not guess these.

The spans can be submitted to any URI path. The path is ignored by the
server and the server always responds with HTTP response code 202 in the
current implementation.

Example server configuration

```erlang
    ...
    {server_zipkin_callback, {otter_server_span_cb, handle}},
    {server_zipkin_port, 19411},
    ...
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
    {zipkin_add_default_service_to_logs, false},
    {zipkin_add_default_service_to_tags, false},
    {server_zipkin_callback, {otter_server_span_cb, handle}},
    {server_zipkin_port, 19411},
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
