%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2018, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2018 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(client_status_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, get_stats/0]).

-define(SERVER, ?MODULE).

-define(MEM(Mem), (Mem) / 1024 / 1024).

%%%===================================================================
%%% API
%%%===================================================================

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init(_) ->
    _ = erlang:system_flag(scheduler_wall_time, true),
    %% _ = msacc:start(),
    Mem = erlang:memory(total),
    SchedWT = lists:sort(erlang:statistics(scheduler_wall_time)),
    {ok, TRef} = timer:send_interval(200, monitor),
    io:format("Monitor started.~n"),
    {ok, #{timer => TRef, sched_wt => SchedWT, mem => Mem}}.

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
handle_call(get_stats, _From, State) ->
    {reply, {ok, State}, State}.

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
handle_cast(_Msg, State) ->
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
handle_info(monitor, #{sched_wt := SchedWTOld}) ->
    {Red, RedNew} = erlang:statistics(reductions),
    {CT, _} = statistics(wall_clock),
    {RT, _} = statistics(runtime),
    {GCsNum, WordsReclaimed, _} = statistics(garbage_collection),
    SchedWTNew = lists:sort(erlang:statistics(scheduler_wall_time)),
    SchedUtil =
        lists:map(
          fun({{I, A0, T0}, {I, A1, T1}}) ->
                  (A1 - A0)/(T1 - T0)
          end, lists:zip(SchedWTOld, SchedWTNew)
         ),
    Mem = erlang:memory(total),
    %% ok = msacc:to_file("/tmp/msacc.out"),
    NewState = #{
      sched_wt => SchedWTNew,
      sched_util_last => SchedUtil,
      mem => Mem,
      rt => RT,
      gc => {GCsNum, WordsReclaimed},
      ct => CT,
      red => Red,
      red_new => RedNew
     },
    %% io:format("stats; red: ~p, red new: ~p, ct: ~p, rt: ~p, mem: ~p(~p), sched: ~p~n",
    %%           [Red, RedNew, CT, RT, ?MEM(Mem0), ?MEM(Mem1 - Mem0), SchedUtil]),
    {noreply, NewState};
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
