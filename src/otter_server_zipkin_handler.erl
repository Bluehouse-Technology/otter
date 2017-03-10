-module(otter_server_zipkin_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, {CbMod, CbFun}} = otter_config:read(server_zipkin_callback),
    {ok, Body, Req1} = cowboy_req:body(Req),
    SpanList = otter_conn_zipkin:decode_spans(Body),
    [CbMod:CbFun(Span) || Span <- SpanList],
    {ok, Req2} = cowboy_req:reply(
        202,
        [
            {<<"X-Application-Context">>, <<"zipkin-server:shared:9411">>}
        ],
        <<>>,
        Req1
    ),
    {ok, Req2, State}.

terminate(_, _, _) ->
    ok.
