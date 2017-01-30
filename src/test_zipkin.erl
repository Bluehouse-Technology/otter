-module(test_zipkin).
-include("zipkin_core_types.hrl").
-include("zipkin_dependencies_types.hrl").

-export([http_test/0]).

http_test() ->
    Body = create_test_span(),
    ibrowse:send_req("http://localhost:9411/api/v1/spans", [], post, Body).

create_test_span() ->
    ParentId = pad16(integer_to_binary(erlang:unique_integer([positive, monotonic]))),
    Spans = lists:map(
	      fun(X) ->
		      X_bin = integer_to_binary(X),
		      Id = pad16(integer_to_binary(erlang:unique_integer([positive, monotonic]))),
		      Duration = rand:uniform(1000),
		      Span = #{<<"id">>   => Id,
			       <<"name">> => <<"test_", X_bin/binary>>,
			       <<"parentId">> => ParentId,
			       <<"traceId">> => ParentId,
			       <<"timestamp">> => timestamp(),
			       <<"duration">> => Duration
			      }
	      end, lists:seq(1, rand:uniform(10))),
    Json = jiffy:encode(Spans),
    io:format("JSON: ~s~n", [Json]),
    Json.

record_to_map(Rec) ->
    Field_names = field_names(Rec),
    Tag = element(1, Rec),
    [_ | Values] = tuple_to_list(Rec),
    maps:from_list([{X, Y} || {X, Y} <- lists:zip(Field_names, Values)]).

field_names(#'Span'{}) ->
    record_info(fields, 'Span').

pad16(X) when is_binary(X), size(X) < 16 ->
    list_to_binary([lists:duplicate(16 - size(X), $0), X]).

timestamp() ->
    erlang:system_time(microsecond).
