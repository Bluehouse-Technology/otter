-module(otter_snap_count).
-compile(export_all).

%% This module implements a simple way of giving operational visibility
%% to events/spans in the system. It expects a Key and Data where the
%% Key is used to increment a counter in a table and also to store the
%% last received Data for that Key. The Data is preferred to be a list
%% of 2 element {K,V} tuples, if it is any other format, it uses
%% [{data, Data}] to store the last information.

snap(Key, [{_, _} |_ ] = Data) ->
    {_, _, Us} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    ets:insert(
        otter_snap_store,
        {
            Key,
            [
                {snap_timestamp, {Year, Month, Day, Hour, Min, Sec, Us}}
                | Data
            ]
        }
    ),
    case catch ets:update_counter(otter_snap_count, Key, 1) of
        {'EXIT', {badarg, _}} ->
            ets:insert(otter_snap_count, {Key, 1});
        Cnt ->
            Cnt
    end;
snap(Key, Data) ->
    snap(Key, [{data, Data}]).

list_counts() ->
    ets:tab2list(otter_snap_count).

get_snap(Key) ->
    ets:lookup(otter_snap_store, Key).

delete_counter(Key) ->
    ets:delete(otter_snap_store, Key),
    ets:delete(otter_snap_count, Key).

delete_all_counters() ->
    ets:delete_all_objects(otter_snap_store),
    ets:delete_all_objects(otter_snap_count).

sup_init() -> [
    ets:new(
        Tab,
        [named_table, public, {Concurrency, true}]
    ) ||
    {Tab, Concurrency} <- [
        {otter_snap_count, write_concurrency},
        {otter_snap_store, write_concurrency}
    ]
].




