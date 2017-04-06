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
