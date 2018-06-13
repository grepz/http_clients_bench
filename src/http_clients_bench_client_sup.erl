-module(http_clients_bench_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Client) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Client]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Client]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    HttpClientTester = {
      http_client_tester, {http_client_tester, start_link, [Client]},
      Restart, Shutdown, Type, []
     },

    StatusMonitor = {
      client_status_monitor, {client_status_monitor, start_link, []},
      Restart, Shutdown, Type, []
     },

    {ok, {SupFlags, [StatusMonitor, HttpClientTester]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
