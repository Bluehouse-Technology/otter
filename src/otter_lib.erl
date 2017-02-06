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
