-ifndef(_zipkin_core_types_included).
-define(_zipkin_core_types_included, yeah).

-define(ZIPKIN_CORE_ANNOTATIONTYPE_BOOL, 0).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_BYTES, 1).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_I16, 2).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_I32, 3).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_I64, 4).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_DOUBLE, 5).
-define(ZIPKIN_CORE_ANNOTATIONTYPE_STRING, 6).

%% struct 'Endpoint'

-record('Endpoint', {'ipv4' :: integer(),
                     'port' :: integer(),
                     'service_name' :: string() | binary(),
                     'ipv6' :: string() | binary()}).
-type 'Endpoint'() :: #'Endpoint'{}.

%% struct 'Annotation'

-record('Annotation', {'timestamp' :: integer(),
                       'value' :: string() | binary(),
                       'host' :: 'Endpoint'()}).
-type 'Annotation'() :: #'Annotation'{}.

%% struct 'BinaryAnnotation'

-record('BinaryAnnotation', {'key' :: string() | binary(),
                             'value' :: string() | binary(),
                             'annotation_type' :: integer(),
                             'host' :: 'Endpoint'()}).
-type 'BinaryAnnotation'() :: #'BinaryAnnotation'{}.

%% struct 'Span'

-record('Span', {'trace_id' :: integer(),
                 'name' :: string() | binary(),
                 'id' :: integer(),
                 'parent_id' :: integer(),
                 'annotations' :: list(),
                 'binary_annotations' :: list(),
                 'debug' = false :: boolean(),
                 'timestamp' :: integer(),
                 'duration' :: integer(),
                 'trace_id_high' :: integer()}).
-type 'Span'() :: #'Span'{}.

-endif.
