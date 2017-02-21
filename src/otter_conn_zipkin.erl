%% This module facilitates encoding/decoding of thrift data lists encoded
%% with the binary protocol, suitable for sending/receiving spans
%% on the zipkin interface.

%% Sending is async

-module(otter_conn_zipkin).
-compile(export_all).
-include("otter.hrl").

sup_init() ->
    [
        ets:new(
            Tab,
            [named_table, public, {Concurrency, true}]
        ) ||
        {Tab, Concurrency} <- [
            {otter_zipkin_buffer1, write_concurrency},
            {otter_zipkin_buffer2, write_concurrency},
            {otter_zipkin_status, read_concurrency}
        ]
    ],
    ets:insert(otter_zipkin_status, {current_buffer, otter_zipkin_buffer1}),
    SendInterval = otter_config:read(zipkin_batch_interval_ms, 100),
    timer:apply_interval(SendInterval, ?MODULE, send_buffer, []).

send_span(Span) ->
    [{_, Buffer}] = ets:lookup(otter_zipkin_status, current_buffer),
    ets:insert(Buffer, Span).

send_spans(Spans) ->
    {ok, ZipkinURI} = otter_config:read(zipkin_collector_uri),
    send_spans(ZipkinURI, Spans).

send_spans(ZipkinURI, Spans) ->
    Data = encode_spans(Spans),
    ibrowse:send_req(
        ZipkinURI,
        [{"content-type", "application/x-thrift"}],
        post,
        Data
    ).

encode_spans(Spans) ->
    encode_implicit_list({struct, [format_span(S) || S <- Spans]}).

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
            case send_spans(Spans) of
                {ok, "202", _, _} ->
                    otter_snap_count:snap(
                        [?MODULE, send_spans, ok],
                        [{spans, length(Spans)}]);
                Error ->
                    otter_snap_count:snap(
                        [?MODULE, send_spans, failed],
                        [
                            {spans, length(Spans)},
                            {error, Error}
                        ])
            end
    end.

format_span(#span{
    id = Id,
    trace_id = TraceId,
    name = Name,
    parent_id = ParentId,
    logs = Logs,
    tags = Tags,
    timestamp = Timestamp,
    duration = Duration
}) ->
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
            format_annotations(Logs)
        }},
        {8, list, {
            struct,
            format_binary_annotations(Tags)
        }},
        {10, i64, Timestamp},
        {11, i64, Duration}
    ].

format_annotations(Logs) ->
    [
        [
            {1, i64, Timestamp},
            {2, string, otter_lib:to_bin(Text)}
        ] ||
        {Timestamp, Text} <- Logs
    ].

%% I think this is silly : for zipkin to accept the span there needs to be
%% at least 1 tag (binary annotation) and each tag needs a host section
%% with a service name and IP/Port.
format_binary_annotations([]) ->
    format_binary_annotations([{<<"dummy_tag_for_zipkin">>, <<"">>}]);
format_binary_annotations(Tags) ->
    HostService = otter_config:read(
        zipkin_tag_host_service,
        atom_to_list(node())
    ),
    HostIP = otter_config:read(zipkin_tag_host_ip, {127,0,0,1}),
    HostPort = otter_config:read(zipkin_tag_host_port, 0),
    [
        [
            {1, string, otter_lib:to_bin(Key)},
            {2, string, otter_lib:to_bin(Value)},
            {3,i32,6},
            {4,struct, [
                {1,i32, otter_lib:ip_to_i32(HostIP)},
                {2,i16, HostPort},
                {3,string, HostService}
            ]}
        ] ||
        {Key, Value} <- Tags
    ].


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