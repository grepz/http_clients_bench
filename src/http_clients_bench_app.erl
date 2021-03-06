%%%-------------------------------------------------------------------
%% @doc http_clients_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(http_clients_bench_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
