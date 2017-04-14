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
%%% @doc This module provides the configuration API but you should
%%% instead use the API in the `otter' module.
%%% @see otter
%%% @end
%%% -------------------------------------------------------------------

-module(otter_config).
-export([
         list/0,
         read/1,
         read/2,
         write/2
        ]).

list() ->
    application:get_all_env(otter).

read(Key) ->
    application:get_env(otter, Key).

read(Key, Default) ->
    application:get_env(otter, Key, Default).

%% This is provided to allow temporary configuration. Obviously in this
%% default implementation it is not persistent as application environment
%% from either the .app file in the ebin directory or from the release
%% specific sys.config (or alike) will be read at startup.
write(Key, Value) ->
    application:set_env(otter, Key, Value).
