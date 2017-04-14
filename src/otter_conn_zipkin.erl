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
%%% @doc This module facilitates encoding/decoding of thrift data
%%% lists encoded with the binary protocol, suitable for
%%% sending/receiving spans on the zipkin interface. Sending spans to
%%% Zipkin is async 
%%% @end
%%% -------------------------------------------------------------------

-module(otter_conn_zipkin).
-export([
         decode_spans/1,
         send_buffer/0,
         store_span/1,
         sup_init/0
        ]).
-include("otter.hrl").

sup_init() ->
    [
        ets:new(
            Tab,
            [named_table, public | TableProps ]
        ) ||
        {Tab, TableProps} <- [
            {otter_zipkin_buffer1, [{write_concurrency, true}, {keypos, 2}]},
            {otter_zipkin_buffer2, [{write_concurrency, true}, {keypos, 2}]},
            {otter_zipkin_status,  [{read_concurrency, true}]}
        ]
    ],
    ets:insert(otter_zipkin_status, {current_buffer, otter_zipkin_buffer1}),
    SendInterval = otter_config:read(zipkin_batch_interval_ms, 100),
    timer:apply_interval(SendInterval, ?MODULE, send_buffer, []).

store_span(Span) ->
    [{_, Buffer}] = ets:lookup(otter_zipkin_status, current_buffer),
    ets:insert(Buffer, Span).

send_buffer() ->
    [{_, Buffer}] = ets:lookup(otter_zipkin_status, current_buffer),
    NewBuffer = case Buffer of
        otter_zipkin_buffer1 ->
            otter_zipkin_buffer2;
        otter_zipkin_buffer2 ->
            otter_zipkin_buffer1
    end,
    ets:insert(otter_zipkin_status, {current_buffer, NewBuffer}),
    case ets:tab2list(Buffer) of
        [] ->
            ok;
        Spans ->
            ets:delete_all_objects(Buffer),
            case send_batch_to_zipkin(Spans) of
                {ok, 202} ->
                    otter_snapshot_count:snapshot(
                        [?MODULE, send_buffer, ok],
                        [{spans, length(Spans)}]);
                Error ->
                    otter_snapshot_count:snapshot(
                        [?MODULE, send_buffer, failed],
                        [
                            {spans, length(Spans)},
                            {error, Error}
                        ])
            end
    end.

send_batch_to_zipkin(Spans) ->
    {ok, ZipkinURL} = otter_config:read(zipkin_collector_uri),
    send_batch_to_zipkin(ZipkinURL, Spans).

send_batch_to_zipkin(ZipkinURL, Spans) ->
    Data = encode_spans(Spans),
    send_spans_http(ZipkinURL, Data).

send_spans_http(ZipkinURL, Data) ->
    send_spans_http(application:get_env(otter, http_client, ibrowse),
		    ZipkinURL, Data).

send_spans_http(ibrowse, ZipkinURL, Data) ->
    case ibrowse:send_req(
        ZipkinURL,
        [{"content-type", "application/x-thrift"}],
        post,
        Data
     ) of
	{ok, SCode, _, _} ->
	    {ok, list_to_integer(SCode)};
	Err ->
	    Err
    end;
send_spans_http(httpc, ZipkinURL, Data) ->
    case httpc:request(post, {ZipkinURL, [], "application/x-thrift", Data}, [], []) of
	{ok, {{_, SCode, _}, _, _}} ->
	    {ok, SCode};
	Err ->
	    Err
    end.

encode_spans(Spans) ->
    encode_implicit_list({struct, [span_to_struct(S) || S <- Spans]}).

decode_spans(Data) ->
    {{struct, StructList}, _Rest} = decode_implicit_list(Data),
    [struct_to_span(S) || S <- StructList].

span_to_struct(#span{
    id = Id,
    trace_id = TraceId,
    name = Name,
    parent_id = ParentId,
    logs = Logs,
    tags = Tags,
    timestamp = Timestamp,
    duration = Duration
}) ->
    FinalTags = case otter_config:read(zipkin_add_host_tag_to_span, undefined) of
        {Key, Value} ->
            [{Key, Value, default} | Tags];
        _ ->
            Tags
    end,
    [
        {1, i64, TraceId},
        {3, string, otter_lib:to_bin(Name)},
        {4, i64, Id}
    ] ++
    case ParentId of
        undefined ->
            [];
        ParentId ->
            [{5, i64, ParentId}]
    end ++
    [
        {6, list, {
            struct,
            [log_to_annotation(Log) || Log <- Logs]
        }},
        {8, list, {
            struct,
            [tag_to_binary_annotation(Tag) || Tag <- FinalTags]
        }},
        {10, i64, Timestamp},
        {11, i64, Duration}
    ].

log_to_annotation({Timestamp, Text}) ->
    case otter_config:read(zipkin_add_default_service_to_logs, false) of
        true ->
            log_to_annotation({Timestamp, Text, default});
        false ->
            [
                {1, i64, Timestamp},
                {2, string, otter_lib:to_bin(Text)}
            ]
    end;
log_to_annotation({Timestamp, Text, Service}) ->
    [
        {1, i64, Timestamp},
        {2, string, otter_lib:to_bin(Text)},
        {3,struct, host_to_struct(Service)}
    ].

tag_to_binary_annotation({Key, Value}) ->
    case otter_config:read(zipkin_add_default_service_to_tags, false) of
        true ->
            tag_to_binary_annotation({Key, Value, default});
        false ->
            [
                {1, string, otter_lib:to_bin(Key)},
                {2, string, otter_lib:to_bin(Value)},
                {3,i32,6}
            ]
    end;
tag_to_binary_annotation({Key, Value, Service}) ->
    [
        {1, string, otter_lib:to_bin(Key)},
        {2, string, otter_lib:to_bin(Value)},
        {3,i32,6},
        {4,struct, host_to_struct(Service)}
    ].

host_to_struct(default) ->
    DefaultService = otter_config:read(
        zipkin_tag_host_service,
        atom_to_list(node())
    ),
    host_to_struct(DefaultService);
host_to_struct(Service) when is_binary(Service) orelse 
                             is_list(Service) orelse
                             is_atom(Service) ->
    host_to_struct({
        otter_lib:to_bin(Service),
        otter_config:read(zipkin_tag_host_ip, {127,0,0,1}),
        otter_config:read(zipkin_tag_host_port, 0)
    });
host_to_struct({Service, Ip, Port}) ->
    [
        {1,i32, otter_lib:ip_to_i32(Ip)},
        {2,i16, Port},
        {3,string, Service}
    ].

struct_to_span(StructData) ->
    struct_to_span(StructData, #span{}).

struct_to_span([{1, i64, TraceId}| Rest], Span) ->
    struct_to_span(Rest, Span#span{trace_id = TraceId});
struct_to_span([{3, string, Name}| Rest], Span) ->
    struct_to_span(Rest, Span#span{name = Name});
struct_to_span([{4, i64, Id}| Rest], Span) ->
    struct_to_span(Rest, Span#span{id = Id});
struct_to_span([{5, i64, ParentId}| Rest], Span) ->
    struct_to_span(Rest, Span#span{parent_id = ParentId});
struct_to_span([{6, list, {struct, Annotations}}| Rest], Span) ->
    Logs = [annotation_to_log(Annotation) || Annotation <- Annotations],
    struct_to_span(Rest, Span#span{logs = Logs});
struct_to_span([{8, list, {struct, BinAnnotations}}| Rest], Span) ->
    Tags = [bin_annotation_to_tag(BinAnnotation) || BinAnnotation <- BinAnnotations],
    struct_to_span(Rest, Span#span{tags = Tags});
struct_to_span([{10, i64, Timestamp}| Rest], Span) ->
    struct_to_span(Rest, Span#span{timestamp = Timestamp});
struct_to_span([{11, i64, Duration}| Rest], Span) ->
    struct_to_span(Rest, Span#span{duration = Duration});
struct_to_span([_ | Rest], Span) ->
    struct_to_span(Rest, Span);
struct_to_span([], Span) ->
    Span.

annotation_to_log(StructData) ->
    annotation_to_log(StructData, {undefined,undefined,undefined}).

annotation_to_log([{1, i64, Timestamp} | Rest], {_, Text, Host}) ->
    annotation_to_log(Rest, {Timestamp, Text, Host});
annotation_to_log([{2, string, Text} | Rest], {Timestamp, _, Host}) ->
    annotation_to_log(Rest, {Timestamp, Text, Host});
annotation_to_log([{3, struct, HostStruct} | Rest], {Timestamp, Text, _}) ->
    annotation_to_log(Rest, {Timestamp, Text, struct_to_host(HostStruct)});
annotation_to_log([_ | Rest], Log) ->
    annotation_to_log(Rest, Log);
annotation_to_log([], {Timestamp, Text, undefined}) ->
    {Timestamp, Text};
annotation_to_log([], Log) ->
    Log.

bin_annotation_to_tag(StructData) ->
    bin_annotation_to_tag(StructData, {undefined,undefined,undefined}).

bin_annotation_to_tag([{1, string, Key} | Rest], {_, Value, Host}) ->
    bin_annotation_to_tag(Rest, {Key, Value, Host});
bin_annotation_to_tag([{2, string, Value} | Rest], {Key, _, Host}) ->
    bin_annotation_to_tag(Rest, {Key, Value, Host});
bin_annotation_to_tag([{4, struct, HostStruct} | Rest], {Key, Value, _}) ->
    bin_annotation_to_tag(Rest, {Key, Value, struct_to_host(HostStruct)});
bin_annotation_to_tag([_ | Rest], Tag) ->
    bin_annotation_to_tag(Rest, Tag);
bin_annotation_to_tag([], {Key, Value, undefined}) ->
    {Key, Value};
bin_annotation_to_tag([], Tag) ->
    Tag.

struct_to_host(StructData) ->
    struct_to_host(StructData, {undefined, undefined, undefined}).

struct_to_host([{1, i32, IntIp} | Rest], {Service, _, Port}) ->
    <<Ip1, Ip2, Ip3, Ip4>> = <<IntIp:32>>,
    struct_to_host(Rest, {Service, {Ip1, Ip2, Ip3, Ip4}, Port});
struct_to_host([{2, i16, Port} | Rest], {Service, Ip, _}) ->
    struct_to_host(Rest, {Service, Ip, Port});
struct_to_host([{3, string, Service} | Rest], {_, Ip, Port}) ->
    struct_to_host(Rest, {Service, Ip, Port});
struct_to_host([_ | Rest], Host) ->
    struct_to_host(Rest, Host);
struct_to_host([], Host) ->
    Host.

%% encode/decode basic thrift binary data
%% e.g. The transport (e.g. HTTP) data is an "implicit" list starting
%% with the element type, and number of elements ..
decode_implicit_list(BinaryData) ->
    decode(list, BinaryData).

encode_implicit_list(Data) ->
    encode({list, Data}).

%% Encoding functions
encode({Id, Type, Data}) ->
    TypeId = map_type(Type),
    EData = encode({Type, Data}),
    <<TypeId, Id:16, EData/bytes>>;
%% .. and without Id (i.e. part of list/set/map)
encode({bool, true}) ->
    <<1>>;
encode({bool, false}) ->
    <<0>>;
encode({byte, Val}) ->
    <<Val>>;
encode({double, Val}) ->
    <<Val:64>>;
encode({i16, Val}) ->
    <<Val:16>>;
encode({i32, Val}) ->
    <<Val:32>>;
encode({i64, Val}) ->
    <<Val:64>>;
encode({string, Val}) when is_list(Val) ->
    Size = length(Val),
    % Might want to convert this to UTF-8 binary first, however for now
    % I'll leave it to the next encoding when binary can be provided in
    % UTF-8 format. In this part is kindly expected it to be ASCII
    % string
    Bytes = list_to_binary(Val),
    <<Size:32, Bytes/bytes>>;
encode({string, Val}) when is_binary(Val) ->
    Size = byte_size(Val),
    <<Size:32, Val/bytes>>;
encode({list, {ElementType, Data}}) ->
    ElementTypeId = map_type(ElementType),
    Size = length(Data),
    EData = list_to_binary([
        encode({ElementType, Element}) ||
        Element <- Data
    ]),
    <<ElementTypeId, Size:32, EData/bytes>>;
encode({set, Data}) ->
    encode({list, Data});
encode({struct, Data}) ->
    EData = list_to_binary([
        encode(StructElement) ||
        StructElement <- Data
    ]),
    <<EData/bytes, 0>>;
encode({map, {KeyType, ValType, Data}}) ->
    KeyTypeId = map_type(KeyType),
    ValTypeId = map_type(ValType),
    Size = length(Data),
    EData = list_to_binary([
        [encode({KeyType, Key}), encode({ValType, Val})] ||
        {Key, Val} <- Data
    ]),
    <<KeyTypeId, ValTypeId, Size:32, EData/bytes>>.

%% Decoding functions
decode(<<TypeId, Id:16, Data/bytes>>) ->
    Type = map_type(TypeId),
    {Val, Rest} = decode(Type, Data),
    {{Id, Type, Val}, Rest}.

decode(bool, <<Val, Rest/bytes>>) ->
    {Val == 1, Rest};
decode(byte, <<Val, Rest/bytes>>) ->
    {Val, Rest};
decode(double, <<Val:64, Rest/bytes>>) ->
    {Val, Rest};
decode(i16, <<Val:16, Rest/bytes>>) ->
    {Val, Rest};
decode(i32, <<Val:32, Rest/bytes>>) ->
    {Val, Rest};
decode(i64, <<Val:64, Rest/bytes>>) ->
    {Val, Rest};
decode(string, <<ByteLen:32, BytesAndRest/bytes>>) ->
    <<Bytes:ByteLen/bytes, Rest/bytes>> = BytesAndRest,
    {Bytes, Rest};
decode(struct, Data) ->
    decode_struct(Data, []);
decode(map, <<KeyTypeId, ValTypeId, Size:32, KVPsAndRest/bytes>>) ->
    decode_map(
        map_type(KeyTypeId),
        map_type(ValTypeId),
        Size,
        KVPsAndRest,
        []
    );
%% Lists and Sets are encoded the same way
decode(set, Data) ->
    decode(list, Data);
decode(list, <<ElementTypeId, Size:32, ElementsAndRest/bytes>>) ->
    decode_list(
        map_type(ElementTypeId),
        Size,
        ElementsAndRest,
        []
    ).

%% Helpers

decode_struct(Data, Acc) ->
    case decode(Data) of
        {Val, <<0, Rest/bytes>>} ->
            {lists:reverse([Val | Acc]), Rest};
        {Val, Rest} ->
            decode_struct(Rest, [Val | Acc])
    end.

decode_map(KeyType, ValType, 0, Rest, Acc) ->
    {{{KeyType, ValType}, list:reverse(Acc)}, Rest};
decode_map(KeyType, ValType, Size, KVPsAndRest, Acc) ->
    {Key, ValAndRest} = decode(KeyType, KVPsAndRest),
    {Val, Rest} =  decode(ValType, ValAndRest),
    decode_map(KeyType, ValType, Size-1, Rest, [{Key, Val} | Acc]).

decode_list(ElementType, 0, Rest, Acc) ->
    {{ElementType, lists:reverse(Acc)}, Rest};
decode_list(ElementType, Size, Elements, Acc) ->
    {Data, Rest} = decode(ElementType, Elements),
    decode_list(ElementType, Size-1, Rest, [Data | Acc]).

map_type(2)     -> bool;
map_type(3)     -> byte;
map_type(4)     -> double;
map_type(6)     -> i16;
map_type(8)     -> i32;
map_type(10)    -> i64;
map_type(11)    -> string;
map_type(12)    -> struct;
map_type(13)    -> map;
map_type(14)    -> set;
map_type(15)    -> list;
map_type(bool)  -> 2;
map_type(byte)  -> 3;
map_type(double)-> 4;
map_type(i16)   -> 6;
map_type(i32)   -> 8;
map_type(i64)   -> 10;
map_type(string)-> 11;
map_type(struct)-> 12;
map_type(map)   -> 13;
map_type(set)   -> 14;
map_type(list)  -> 15.
