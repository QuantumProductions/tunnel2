-module(actions).
-behaviour(gen_server).
-compile(export_all).

advancePlayers(NewCurrent, NewOther, NextCurrentPlayer) ->
  Map = maps:new(),
  Map2 = maps:put(NextCurrentPlayer, NewCurrent, Map),
  maps:put(otherPlayer(NextCurrentPlayer), NewOther, Map2).

move({start, Slices, RecentTaken}, {Players, Current}) ->
  CurrentPlayer = maps:get(Current, Players),
  OtherPlayer = maps:get(otherPlayer(Current), Players),
  case move(slices, Slices, RecentTaken, CurrentPlayer, OtherPlayer) of
    {ok, {NewCurrent, NewOther}} -> 
      {ok, {advancePlayers(NewCurrent, NewOther, Current), Current}};
    {swap, {NewCurrent, NewOther}} -> 
      NewPlayer = otherPlayer(Current),
      {swap, {advancePlayers(NewCurrent, NewOther, NewPlayer), NewPlayer}}
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
handle_call({current, Player}, _From, {Players, Current}) ->
  {reply, Player =:= Current, {Players, Current}};  
handle_call(restart, _, _) ->
  NewState = defaultState(),
  {reply, NewState, NewState};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
  % API for {move, how many branches sliced, recent tile taken true or false}
handle_call({move, Slices, Recent}, _From, State) ->
  {Status, State2} = move({start, Slices, Recent}, State),
  {reply, {Status, State2}, State2};
handle_call(_, _, State) ->
  {reply, State, State}.

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

go() ->
  gen_server:start_link(?MODULE, [], []).

defaultState() ->
  {defaultPlayers(), x}.

init([]) -> 
  {ok, defaultState()}.


% account for bonus AP
% keywords will work here.