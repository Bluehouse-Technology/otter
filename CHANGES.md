#Changes

## 0.2.0 - 0.3.0

###Separate common otter functions to **otter_lib** application
This change is part of the implementation of issue #16

#### span data structure wrapper *otter_lib_span*
This module exports wrapper functions around the span data structure

#### filter library *otter_lib_filter*
The filter library exports functions **run/2** and **run/3**. The functions
take Tags (Key-Value pairs), Rules (list of Condition-Action pairs) and
optionally a flag (the atom *break* or the atom *continue*) to determine
the filtering behaviour. If the rule evaluation should stop at the first
match then the *break* flag should be provided. If multiple matches are
allowed i.e. the whole set of Condition-Action pairs to be evaluated and
the matching actions collected, then the *continue* flag should be given.
**run/2** uses *continue* as default filter behaviour. The filter returns
the list of collected actions that can be any terms specified in the Action
list for a matching condition. The filter library implements the conditions
but the execution of the collected actions is left to the caller.

#### thrift codec library *otter_lib_thrift*
The library contains generic encoding/decoding functions for thrift binary
protocol. Check the module source for more details on the data structures
used/generated.

#### zipkin thrift encoding/decoding *otter_lib_zipkin_thrift*
Encoding/decoding otter span to/from zipkin binary thrift messages. This
module builds on *otter_lib_thrift*

#### snapshot/count functions *otter_lib_snapshot_count*
Basic functions for counting in ETS table as well as storing a term for
each key for counts. The idea behind the mechanism is that in many cases
counting and logging apply to the same events in the code. In cases when
logging of individual counted events is not necessary but it is operationally
useful to retain data about the last event that increased a counter
(e.g. an error or simply get a sample data of some operation) then this
can be used as a fairly efficient solution.
The module uses ETS tables to count and store terms. It is made as a passive
library so in order to initialize the public ETS tables, *sup_init/0*
should be called from a persistent process (e.g. an application supervisor).
The module also contains functions to read/delete the counters and snapshots.

###Remove cowboy as a dependency
Cowboy was used as a dependency only for testing purposes. With the separation
of **otter_lib** application, the server functionality is being moved to
another application **otter_server** which then can be used as testing
dependency for otter if needed. This change is part of the fix for issue #16 .

###Create new API module **otter_span_mpdict_api**
New otter API for span generation. This implements the mechanism described
in issue #14.

###Create new API module **otter_span_id_api**
New otter API for span generation. This implements the separate span
process mechanism described in issue #13.

###Allow external filter engine
In this version the configuration parameter **filter_rules** can also
have the value **{Module, Function, ExtraArg}**. If this format is used
then the following call is made for each span.

```erlang
{NewSpan, ActionList} = Module:Function(Span, ExtraArg),
...
```

The callback is expected to return a new span (i.e. it can modify the
span if neded) and a list of actions that are understood by otter.
Any other return value means that the span will be discarded.
Currently otter supports actions **{snapshot_count, Key}** and **send_to_zipkin**.
Any other term not understood by otter as an action is ignored.

###New rule condition **{one_out_of, N}**
The otter filter library now contains an additional condition **{one_out_of, N}**.
This condition uses crypto:rand_uniform(0, N) and if the generated number is 0
then returns true, otherwise returns false.

