-module(otter_filter).
-export([span/1]).

-include_lib("otter_lib/src/otter.hrl").

-spec span(span()) -> ok.
span(Span) ->
    case otter_config:read(filter_rules, []) of
        [{_,_}|_] = Rules ->
            run_rules(Span, Rules);
        {Module, Function, ExtraArgs} ->
            {NewSpan, Actions} = Module:Function(Span, ExtraArgs),
            [action(NewSpan, Action) || Action <- Actions],
            ok;
        [] ->
            ok
    end.

run_rules(Span, Rules) ->
    Tags = make_rule_tags(Span),
    Actions = otter_lib_filter:run(Tags, Rules, break),
    do_actions(Span, Tags, Actions).

do_actions(Span, Tags, [{snapshot_count, Prefix, TagNames} | Rest]) ->
    SnapCountKey = Prefix ++ [
        case lists:keyfind(Key, 1, Tags) of
            {Key, Value} -> Value;
            _ -> undefined
        end ||
        Key <- TagNames
    ],
    action(Span, {snapshot_count, SnapCountKey}),
    do_actions(Span, Tags, Rest);
do_actions(Span, Tags, [Action | Rest]) ->
    action(Span, Action),
    do_actions(Span, Tags, Rest);
do_actions(_Span, _Tags, []) ->
    ok.

action(Span, {snapshot_count, Key}) ->
    otter_lib_snapshot_count:snapshot(Key, Span);
action(Span, send_to_zipkin) ->
    otter_conn_zipkin:store_span(Span);
action(Span, UnknownAction) ->
    otter_lib_snapshot_count:snapshot(unknown_filter_action, {UnknownAction, Span}),
    unknown_filter_action.

make_rule_tags(#span{tags = Tags, name = Name, duration = Duration}) ->
    [
        {otter_span_name, Name},
        {otter_span_duration, Duration}|
        Tags
    ].
