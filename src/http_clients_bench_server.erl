%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2018, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2018 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(http_clients_bench_server).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-define(PORT, 8080).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_, _) ->
    ok.

start() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Routes = [{'_', [{"/fake", [], http_clients_bench, []}]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(
                http, 100, [{port, ?PORT}],
                [{env, [{dispatch, Dispatch}]}, {max_keepalive, infinity}]
               ),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
