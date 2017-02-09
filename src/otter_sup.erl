-module(otter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    [M:sup_init() || M <- [otter_snap_count, otter_conn_zipkin]],
    {ok, { {one_for_all, 0, 1}, []} }.
