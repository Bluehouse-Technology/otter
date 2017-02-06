%% The purpose of this module is to have a layer of abstraction for
%% configuration. Environment specific (e.g. mnesia based or other)
%% implementations can be done by replacing this module.
-module(otter_config).
-compile(export_all).


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
