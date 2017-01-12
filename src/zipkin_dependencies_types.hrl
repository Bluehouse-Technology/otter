-ifndef(_zipkin_dependencies_types_included).
-define(_zipkin_dependencies_types_included, yeah).

%% struct 'DependencyLink'

-record('DependencyLink', {'parent' :: string() | binary(),
                           'child' :: string() | binary(),
                           'callCount' :: integer()}).
-type 'DependencyLink'() :: #'DependencyLink'{}.

%% struct 'Dependencies'

-record('Dependencies', {'start_ts' :: integer(),
                         'end_ts' :: integer(),
                         'links' :: list()}).
-type 'Dependencies'() :: #'Dependencies'{}.

-endif.
