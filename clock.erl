-module(clock).
-compile(export_all).
-behaviour(gen_server).
-define(INTERVAL, 250).
-define(STARTING, 24000).
-define(BONUS, 0).

run(Pid, Then) ->
  Delta = timer:now_diff(erlang:timestamp(), Then) / 10000,
  case s:s(Pid, {tick, Delta}) of
    {Times, Current, timeout} ->
      {Times, Current, timeout};
    _ -> 
      timer:apply_after(?INTERVAL, ?MODULE, run, [Pid, erlang:timestamp()])
  end.

startingTimes() ->
  Map = maps:new(),
  MapX = maps:put(x, ?STARTING, Map),
  maps:put(o, ?STARTING, MapX).

tick({Times, Current, over}, _Delta) ->
  {Times, Current, over};
tick({Times, Current, finished, Postgame}, Delta) ->
  NextTime = Postgame + Delta,
  case NextTime > 5000 of
    true -> {Times, Current, over};
    false -> {Times, Current, finished, NextTime}
  end;
  
tick({Times, Current, _Status}, Delta) ->
  Time = maps:get(Current, Times),
  Now = Time - Delta,
  case Now of
    Now when Now > 0 ->
      Times2 = maps:put(Current, Now, Times),
      {Times2, Current, started};
    _ ->
      {Times, Current, timeout}
  end.

otherPlayer(x) -> o;
otherPlayer(o) -> x.

swap({Times, Current, timeout}) ->
  {Times, Current, timeout};
swap({Times, Current, unstarted}) ->
  {ok, _Tref} = timer:apply_after(?INTERVAL, ?MODULE, run, [self(), erlang:timestamp()]),
  {Times, otherPlayer(Current), started};
swap({Times, Current, Status}) ->
  {Times, otherPlayer(Current), Status}.

handle_call(info, _, State) ->
  {reply, State, State};
handle_call(swap, _, State) ->
  NewState = swap(State),
  {reply, NewState, NewState};
handle_call(finish, _, {Times, Current, _}) ->
  State2 = {Times, Current, finished, 0},
  {reply, State2, State2};
handle_call({tick, Delta}, _, State) ->
  case tick(State, Delta) of
    timeout -> {reply, timeout, timeout};
    NewState -> {reply, NewState, NewState}
  end.

defaultState() ->
  {startingTimes(), x, unstarted}.

init([]) -> 
  {ok, defaultState()}.

terminate(normal, State) ->
    io:format("Clock.~p~n", [State]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
handle_cast(_, State) ->
    {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

go() ->
  gen_server:start_link(?MODULE, [], []).
