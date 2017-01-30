-module(otter_api).

-record(span, {name, start_ts, context, tags, logs, refs, end_ts}).

-record(span_context, {trace_id   :: pos_integer(), 
		       span_id    :: pos_integer(), 
		       is_sampled :: boolean(), 
		       baggage    :: #{key => binary(), value => binary()}
		      }).

-record(tracer, {name, options, state, mod, func}).

-spec start_span(Operation_name :: binary(), Options :: #{}) -> #span{}.
start_span(_, _) ->
    tbd.

-spec start_span_from_context(#span_context{}, Operation_name :: binary()) -> #span{}.
start_span_from_context(_, _) ->
    tbd.
    
-spec finish_span(Span :: #span{}) -> ok.
finish_span(_) ->
    tbd.

-spec new_tracer(Tracer_name :: atom(), Tracer_options :: #{}) -> #tracer{}.
new_tracer(_, _) ->
    tbd.



