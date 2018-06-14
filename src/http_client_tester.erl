%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2018, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2018 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(http_client_tester).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONC, 100).
-define(END_CYCLE, 5000).
-define(URL, "http://127.0.0.1:8080/fake").

-define(TEST(Cycle), {run, Cycle}).

%%%===================================================================
%%% API
%%%===================================================================

test(Cycle) ->
    ok = gen_server:cast(?MODULE, ?TEST(Cycle)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Client) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Client], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Client]) ->
    io:format("Starting client tester.~n"),
    {ok, OutFd} = file:open("stats.output", write),
    case Client of
        hackney -> prepare_hackney();
        ibrowse -> prepare_ibrowse()
    end,
    Time = erlang:timestamp(),
    ok = test(0),
    {ok, #{out => OutFd, client => Client, wait => undefined, cycle => undefined, result => undefined, time => Time}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(?TEST(?END_CYCLE), State) ->
    erlang:halt(),
    {noreply, State};
handle_cast(?TEST(Cycle), #{out := OutFd, client := Client, result := Result, time := Time0} = State) ->
    TDiff = timer:now_diff(erlang:timestamp(), Time0),
    case client_status_monitor:get_stats() of
        {ok, #{
           sched_util_last := SchedUtil,
           mem := Mem,
           rt := RT,
           ct := CT,
           red := Red,
           gc := {GCs, WordsReclaimed},
           red_new := RedNew
          }} ->
            SchedAvrg = avrg(SchedUtil),
            Succ =
                case Result of
                    {Ok, Err} -> Ok - Err;
                    _ -> 0
                end,
            io:format("CYCLE: ~p; RESULT: ~p; MEM: ~p; TIME: ~p; RT: ~p; CT: ~p; RED: ~p; RED_NEW: ~p; SCHED: ~p; GCs: ~p; GC_RCLMD: ~p\n", [Cycle, Result, Mem, TDiff, RT, CT, Red, RedNew, SchedAvrg, GCs, WordsReclaimed]),
            io:format(OutFd, "~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p\n", [Cycle, Succ, Mem, TDiff, RT, CT, Red, RedNew, SchedAvrg, GCs, WordsReclaimed]);
        _ ->
            io:format("cycle: ~p; ...~n", [Cycle])
    end,
    case Client of
        hackney ->
            ok = run(fun hackney_req/1, Cycle, ?CONC);
        ibrowse ->
            ok = run(fun ibrowse_req/1, Cycle, ?CONC)
    end,
    Time1 = erlang:timestamp(),
    {noreply, State#{wait => ?CONC, result => {0, 0}, cycle => Cycle, time => Time1}};
handle_cast(_, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(
  {result, Cycle, Res},
  #{cycle := Cycle, wait := Wait0, result := {Ok, Err}} = State
 ) ->
    ResultNew =
        case Res of
            ok -> {Ok + 1, Err};
            error -> {Ok, Err +1}
        end,
    Wait1 = Wait0 - 1,
    case Wait1 of
        0 -> ok = test(Cycle + 1);
        _ -> ok
    end,
    {noreply, State#{wait => Wait1, result => ResultNew}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run(_, _, 0) -> ok;
run(Fun, Cycle, Conc) ->
    spawn(
      fun () ->
              Res =
                  try ok = Fun(Cycle)
                  catch _ -> error
                  end,
              ?MODULE ! {result, Cycle, Res}
      end),
    run(Fun, Cycle, Conc - 1).

hackney_req(Cycle) ->
    {ok, 200, _Headers, Ref} =
        hackney:request(get, ?URL,
                        [], integer_to_binary(Cycle), []%% [{pool, hackney_pool}]
                       ),
    {ok, _} = hackney:body(Ref),
    ok.

ibrowse_req(Cycle) ->
    case ibrowse:send_req(?URL, [], get, [as_binary(Cycle)], [], infinity) of
        {ok, "200", _, _} -> ok;
        {error, retry_later} ->
            timer:sleep(1),
            ibrowse_req(Cycle)
    end.

avrg(Sched) ->
    N0 = length(Sched),
    {Cum, N1} =
        lists:foldl(
          fun (V, {Cum, N}) ->
                  case V == 0 of
                      true -> {Cum, N - 1};
                      false -> {Cum + V, N}
                  end
          end, {0, N0}, Sched),
    Cum / N1.


prepare_hackney() ->
    {ok, _} = application:ensure_all_started(hackney).
    %% Options = [{timeout, 5000}, {max_connections, 300}],
    %% ok = hackney_pool:start_pool(hackney_pool, Options).

prepare_ibrowse() ->
    _ = ibrowse:start(),
    ok.

as_binary(Integer) when is_integer(Integer) ->
	as_binary(integer_to_list(Integer));

as_binary(List) when is_list(List) ->
	list_to_binary(List);

as_binary(Atom) when is_atom(Atom) ->
	atom_to_binary(Atom, latin1);

as_binary(Binary) when is_binary(Binary) ->
	Binary;

as_binary(_Any) ->
	throw(badarg).
