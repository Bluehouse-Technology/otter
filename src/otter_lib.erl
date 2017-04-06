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
%%%-------------------------------------------------------------------

-module(otter_lib).
-compile(export_all).


ip_to_i32({A,B,C,D}) ->
    <<Ip:32>> = <<A,B,C,D>>,
    Ip.

i32_to_ip(<<A,B,C,D>>) ->
    {A,B,C,D}.

timestamp() ->
    {MeS, S, MuS} = os:timestamp(),
    (MeS*1000000+S)*1000000+MuS.

id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    Id.

to_bin(Int) when is_integer(Int)->
    integer_to_binary(Int);
to_bin(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, 'utf8');
to_bin(List) when is_list(List) ->
    unicode:characters_to_binary(List);
to_bin(Binary) when is_binary(Binary) ->
    Binary;
to_bin(Fun) when is_function(Fun, 0) ->
    to_bin(Fun());
to_bin(Value) ->
    unicode:characters_to_binary(io_lib:format("~1024p", [Value])).
