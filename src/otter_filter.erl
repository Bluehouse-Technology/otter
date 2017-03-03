-module(otter_filter).
-compile(export_all).
-include("otter.hrl").

%% The main idea behind this filter that the processing of the spans can
%% be modified runtime by changing the filter configuration. This way
%% logging, counting or sending data to trace collectors can be modified
%% on the running system based on the changing operational requirements.

%% The span filter works with key value pair lists easiest to implement
%% and reasonably fast.

%% Filter rules are composed by a list of {Conditions, Actions} tuples.
%% Processing a span means iterating through this list and when an item
%% found where all Conditions evaluate to true then the Actions in that
%% item are executed.

%% Conditions operate on a copy of the tags of the span. It is a
%% sequence of checks against the tags (e.g. key present, key value)
%% where if any check in the sequence fails, the associated actions are
%% not executed and the next {Conditions, Actions} item is evaluated.
%% Actions can trigger e.g. counting a particular tag value combination,
%% logging, sending the span to trace collectors, modifying the working
%% (i.e. used for further conditions) tag list, modifying the span tag
%% list that is to be sent to trace collector. Actions can also
%% influence the further evaluation of the rules i.e. providing the
%% break action instructs the rule engine NOT to look further in the
%% list of {Conditions, Actions}.

%% Evaluation of the rules happens in the process which invokes the span
%% end statement (e.g. otter_span:pend/0) i.e. it has impact on the
%% request processing time. Therefore the actions that consume little
%% time and resources with no external interfaces (e.g. counting in ets)
%% can be done during the evaluation of the rules, but anything that has
%% external interface or dependent on environment (e.g. logging and trace
%% collecting) should be done asynchronously.

span(#span{tags = Tags, name = Name, duration = Duration} = Span) ->
    Rules = otter_config:read(filter_rules, []),
    rules(Rules, [
        {otter_span_name, Name},
        {otter_span_duration, Duration}|
        Tags
    ], Span).

rules([{Conditions, Actions} | Rest], Tags, Span) ->
    case check_conditions(Conditions, Tags) of
        false ->
            rules(Rest, Tags, Span);
        true ->
            case do_actions(Actions, Tags, Span) of
                break ->
                    ok;
                {continue, NewTags, NewSpan} ->
                    rules(Rest, NewTags, NewSpan)
            end
    end;
rules(_, _, _) ->
    ok.

check_conditions([Condition | Rest], Tags) ->
    case check(Condition, Tags) of
        true ->
            check_conditions(Rest, Tags);
        false ->
            false
    end;
check_conditions([], _) ->
    true.

check({negate, Condition}, Tags) ->
    not check(Condition, Tags);
check({value, Key, Value}, Tags) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, Value} ->
            true;
        _ ->
            false
    end;
check({same, Key1, Key2}, Tags) ->
    case lists:keyfind(Key1, 1, Tags) of
        {Key1, Value} ->
            case lists:keyfind(Key2, 1, Tags) of
                {Key2, Value} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end;
check({greater, Key, Value}, Tags) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, Value1} when Value1 > Value  ->
            true;
        _ ->
            false
    end;
check({less, Key, Value}, Tags) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, Value1} when Value1 < Value  ->
            true;
        _ ->
            false
    end;
check({between, Key, Value1, Value2}, Tags) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, Value} when Value > Value1 andalso Value < Value2  ->
            true;
        _ ->
            false
    end;
check({present, Key}, Tags) ->
    lists:keymember(Key, 1, Tags);
check(_, _) ->
    false.

do_actions(Actions, Tags, Span) ->
    do_actions(Actions, Tags, Span, continue).

do_actions([break | Rest], Tags, Span, _BreakOrContinue) ->
    do_actions(Rest, Tags, Span, break);
do_actions([continue | Rest], Tags, Span, _BreakOrContinue) ->
    do_actions(Rest, Tags, Span, continue);
do_actions([Action | Rest], Tags, Span, BreakOrContinue) ->
    do_actions(Rest, action(Action, Tags, Span), Span, BreakOrContinue);
do_actions([], _Tags, _Span, break) ->
    break;
do_actions([], Tags, Span, continue) ->
    {continue, Tags, Span}.

action(send_to_zipkin, Tags, Span) ->
    otter_conn_zipkin:store_span(Span),
    Tags;
action({snapshot_count, Prefix, TagNames}, Tags, Span) ->
    TagValues = [
        case lists:keyfind(Key, 1, Tags) of
            {Key, Value} -> Value;
            _ -> undefined
        end ||
        Key <- TagNames
    ],
    otter_snapshot_count:snapshot(Prefix ++ TagValues, Span),
    Tags;
action(_, Tags, _) ->
    Tags.
