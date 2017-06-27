-module(actions).
-behaviour(gen_server).
-compile(export_all).
-define(INTERVAL, 250).
-define(STARTING, 2400).
 % -define(STARTING, 2).
-define(BONUS, 0).
-define(TIMEOUT, 600).

advancePlayers(NewCurrent, NewOther, NextCurrentPlayer) ->
  Map = maps:new(),
  Map2 = maps:put(NextCurrentPlayer, NewCurrent, Map),
  maps:put(otherPlayer(NextCurrentPlayer), NewOther, Map2).
  % update existing?

move({start, Slices, RecentTaken}, {Players, Current}) ->
  CurrentPlayer = maps:get(Current, Players),
  OtherPlayer = maps:get(otherPlayer(Current), Players),
  case move(slices, Slices, RecentTaken, CurrentPlayer, OtherPlayer) of
    {ok, {NewCurrent, NewOther}} -> 
      {advancePlayers(NewCurrent, NewOther, Current), Current};
    {swap, {NewCurrent, NewOther}} -> 
      NewPlayer = otherPlayer(Current),
      {advancePlayers(NewCurrent, NewOther, NewPlayer), NewPlayer}
  end.
move(slices, Slices, RecentTaken, {Active, Next}, OtherPlayer) ->
  move(recent, RecentTaken, {Active - 1, Next + sliceBonus(Slices)}, OtherPlayer).
move(recent, RecentTaken, CurrentPlayer, {0, Next}) ->
  move(advance, CurrentPlayer, {0, Next + recentBonus(RecentTaken)}).
move(advance, {0, Next}, {0, ONext}) ->
  {swap, {{maxMove(ONext + 2), 0}, {0, Next}}};
move(advance, CurrentPlayer, OtherPlayer) ->
  {ok, {CurrentPlayer, OtherPlayer}}.

maxMove(N) when N >= 4 -> 4;
maxMove(N) -> N.

otherPlayer(x) -> o;
otherPlayer(o) -> x.

defaultPlayers() ->
  Map = maps:new(),
  MapX = maps:put(x, {1, 0}, Map),
  maps:put(o, {0, 0}, MapX).

sliceBonus(0) ->
  0;
sliceBonus(N) ->
  N.

recentBonus(false) ->
  0;
recentBonus(true) ->
  1.


handle_call(info, _From, State) ->
  {reply, State, State};
handle_call({current, Player}, _From, State = {_Players, Current, _Clock, _CallbackPid}) ->
  {reply, Player =:= Current, State};  
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call({move, Slices, Recent}, _From, {Players, Current, Clock, CallbackPid}) ->
  {Players2, Current2} = move({start, Slices, Recent}, {Players, Current}),
  State2 = {Players2, Current2, Clock, CallbackPid},
  {reply, State2, State2};
% todo: set over
% todo refactor timeout into over
handle_call({tick, Delta}, _, {Players, Current, {Times, {over, Amount}}, CallbackPid}) ->
  case Amount of
    Enough when Enough >= ?TIMEOUT ->
      s:s(CallbackPid, over);
    _NotEnough ->
      timer:apply_after(?INTERVAL, ?MODULE, run, [self, erlang:timestamp()]),
      {noreply, {Players, Current, {Times, {over, Amount + Delta}}, CallbackPid}}
  end;
handle_call({tick, Delta}, _, {Players, Current, Clock, CallbackPid}) ->
  NewClock = newClock(Clock, Delta, Current),
  case NewClock of
    {_Times2, timeout} ->
      s:s(CallbackPid, {timeout, Current});
    {_Times2, started} ->
      timer:apply_after(?INTERVAL, ?MODULE, run, [self, erlang:timestamp()])
  end,
  State2 = {Players, Current, NewClock, CallbackPid},
  {noreply, State2};

handle_call(_, _, State) ->
  {reply, State, State}.

% clock
startingClock() ->
  {startingTimes(), started}.

swap({Times, timeout}) ->
  {Times, timeout};
swap({Times, Status}) ->
  {Times, Status}.

run(Pid, Then) ->
  Delta = timer:now_diff(erlang:timestamp(), Then) / 10000,
  s:s(Pid, {tick, Delta}).

% clock tick
newClock({Times, {over, Amount}}, Delta, _Current) ->
  {Times, {over, Amount + Delta}};
newClock({Times, timeout}, _Delta, _Current) ->
  {Times, timeout};
newClock({Times, started}, Delta, Current) ->
  Time = maps:get(Current, Times),
  Now = Time - Delta,
  case Now of
    Now when Now > 0 ->
      Times2 = maps:put(Current, Now, Times),
      {Times2, started};
    _ ->
      {Times, timeout}
  end.

startingTimes() ->
  Map = maps:new(),
  MapX = maps:put(x, ?STARTING, Map),
  maps:put(o, ?STARTING, MapX).

terminate(normal, State) ->
    io:format("Actions.~p~n", [State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

go(CallbackPid) ->
  {ok, Pid} = gen_server:start_link(?MODULE, CallbackPid, []),
  timer:apply_after(?INTERVAL, ?MODULE, run, [Pid, erlang:timestamp()]),
  {ok, Pid}.

init(CallbackPid) -> 
  {ok, defaultState(CallbackPid)}.

defaultState(CallbackPid) ->
  {defaultPlayers(), x, startingClock(), CallbackPid}.