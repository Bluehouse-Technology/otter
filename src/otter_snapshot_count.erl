%%%-------------------------------------------------------------------
%%% Licensed to the Apache Software Foundation (ASF) under one
%%% or more contributor license agreements.  See the NOTICE file
%%% distributed with this work for additional information
%%% regarding copyright ownership.  The ASF licenses this file
%%% to you under the Apache License, Version 2.0 (the
%%% "License"); you may not use this file except in compliance
%%% with the License.  You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% @doc
%%% This module implements a simple way of giving operational visibility
%%% to events/spans in the system. It expects a Key and Data where the
%%% Key is used to increment a counter in a table and also to store the
%%% last received Data for that Key. The Data is preferred to be a list
%%% of 2 element {K,V} tuples, if it is any other format, it uses
%%% [{data, Data}] to store the last information.
%%% @end
%%%-------------------------------------------------------------------

-module(otter_snapshot_count).
-export([
         delete_all_counters/0,
         delete_counter/1,
         get_snap/1,
         list_counts/0,
         snapshot/2,
         sup_init/0
        ]).


snapshot(Key, [{_, _} |_ ] = Data) ->
    {_, _, Us} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    ets:insert(
        otter_snapshot_store,
        {
            Key,
            [
                {snap_timestamp, {Year, Month, Day, Hour, Min, Sec, Us}}
                | Data
            ]
        }
    ),
    case catch ets:update_counter(otter_snapshot_count, Key, 1) of
        {'EXIT', {badarg, _}} ->
            ets:insert(otter_snapshot_count, {Key, 1});
        Cnt ->
            Cnt
    end;
snapshot(Key, Data) ->
    snapshot(Key, [{data, Data}]).

list_counts() ->
    ets:tab2list(otter_snapshot_count).

get_snap(Key) ->
    ets:lookup(otter_snapshot_store, Key).

delete_counter(Key) ->
    ets:delete(otter_snapshot_store, Key),
    ets:delete(otter_snapshot_count, Key),
    ok.

delete_all_counters() ->
    ets:delete_all_objects(otter_snapshot_store),
    ets:delete_all_objects(otter_snapshot_count),
    ok.

sup_init() -> 
    [
     ets:new(Tab, [named_table, public, {Concurrency, true}]) ||
        {Tab, Concurrency} <- [
                               {otter_snapshot_count, write_concurrency},
                               {otter_snapshot_store, write_concurrency}
                              ]
    ].
