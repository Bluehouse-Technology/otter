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
    list_to_binary(atom_to_list(Atom));
% Well, this is not so obvious so I assume the warnings in documentation
% work and folks will send character listst. Otherwise it should crash
% somewhere.
to_bin(List) when is_list(List) ->
    list_to_binary(List);
to_bin(Binary) when is_binary(Binary) ->
    Binary.
