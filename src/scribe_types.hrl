-ifndef(_scribe_types_included).
-define(_scribe_types_included, yeah).

-define(SCRIBE_RESULTCODE_OK, 0).
-define(SCRIBE_RESULTCODE_TRY_LATER, 1).

%% struct 'LogEntry'

-record('LogEntry', {'category' :: string() | binary(),
                     'message' :: string() | binary()}).
-type 'LogEntry'() :: #'LogEntry'{}.

-endif.
