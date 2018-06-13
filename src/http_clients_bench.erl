-module(http_clients_bench).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    ok = timer:sleep(5),
    {ok, Req2} = cowboy_req:reply(200, [
            {<<"content-type">>, <<"text/plain">>}
    ], <<"reply">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
