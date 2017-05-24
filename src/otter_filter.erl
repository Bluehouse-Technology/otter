-module(otter_filter).
-export([span/1, pre_span/1]).

-include_lib("otter_lib/src/otter.hrl").

%%----------------------------------------------------------------------
%% @doc Invoke the span filter on active span
%% @end
%%----------------------------------------------------------------------
-spec span(Span :: span()) -> span().
span(#span{timestamp = 0} = Span) ->
    Span;
span(Span) ->
    run(span, Span).

%%----------------------------------------------------------------------
%% @doc Invoke the span pre filter. If the filter result is to discard
%% the span in this phase (when the start_with_tags/1 API function is
%% called), then the timestamp or the span is set to 0, indicating that
%% the span is not active.
%% @end
%%----------------------------------------------------------------------
-spec pre_span(Span :: span()) -> span().
pre_span(Span) ->
    run(prefilter, Span).


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
run(Type, Span) ->
    Config = case Type of
        prefilter -> prefilter_rules;
        _ -> filter_rules
    end,
    case otter_config:read(Config, []) of
        [{_,_}|_] = Rules ->
            run_rules(Type, Span, Rules);
        {Module, Function, ExtraArgs} ->
            {NewSpan, Actions} = Module:Function(Span, ExtraArgs),
            do_actions(Type, NewSpan, make_tags(NewSpan), Actions, Span);
        [] ->
            Span
    end.

run_rules(Type, Span, Rules) ->
    BreakOrContinue = case Type of prefilter -> break; _ -> continue end,
    Tags = make_tags(Span),
    Actions = otter_lib_filter:run(Tags, Rules, BreakOrContinue),
    do_actions(Type, Span, Tags, Actions, Span).

do_actions(Type, Span, Tags, [{snapshot_count, Prefix, TagNames} | Rest], Return) ->
    SnapCountKey = Prefix ++ [
        case lists:keyfind(Key, 1, Tags) of
            {Key, Value} -> Value;
            _ -> undefined
        end ||
        Key <- TagNames
    ],
    otter_lib_snapshot_count:snapshot(SnapCountKey, Span),
    do_actions(Type, Span, Tags, Rest, Return);
do_actions(prefilter, Span, Tags, [allow | Rest], _Return) ->
    do_actions(prefilter, Span, Tags, Rest, Span);
do_actions(prefilter, Span, Tags, [discard | Rest], _Return) ->
    do_actions(prefilter, Span, Tags, Rest, deactivate_span(Span));
do_actions(span, Span, Tags, [send_to_zipkin | Rest], Return) ->
    otter_conn_zipkin:store_span(Span),
    do_actions(span, Span, Tags, Rest, Return);
do_actions(Type, Span, Tags, [UnknownAction| Rest], Return) ->
    otter_lib_snapshot_count:snapshot(unknown_filter_action, {UnknownAction, Span}),
    do_actions(Type, Span, Tags, Rest, Return);
do_actions(_Type, _Span, _Tags, [], Return) ->
    Return.

make_tags(#span{tags = Tags, name = Name, duration = Duration}) ->
    [
        {otter_span_name, Name},
        {otter_span_duration, Duration}|
        Tags
    ].


%% In case of prefiltering return a span with timestamp set to 0
deactivate_span(Span) ->
    Span#span{timestamp = 0}.
