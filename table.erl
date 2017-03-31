-module(table).
-compile(export_all).

processResult(State = {#{status := playing}, _BoardPid, ActionsPid}, {ok, NewBoard, Slices, RecentTaken}) ->
  s:s(ActionsPid, {move, Slices, RecentTaken}),
  {{ok, NewBoard}, State};
processResult({#{status := playing}, BoardPid, ActionsPid}, {_Won, NewBoard, Slices, RecentTaken}) ->
  s:s(ActionsPid, {move, Slices, RecentTaken}),
  {{won, NewBoard}, {#{status => won}, BoardPid, ActionsPid}};
processResult(State, {error, Error, _Board}) ->
  {{error, Error}, State}.

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({place, Action, Player, Position}, _, State = {#{status := _}, BoardPid, ActionsPid}) ->
  {_, CurrentPlayer} = s:s(ActionsPid, info),
  Result = s:s(BoardPid, {place, CurrentPlayer, {Action, Player, Position}}),
  {Response, State2} = processResult(State, Result),
  {reply, Response, State2};  
handle_call(info, _, State = {#{status := Status}, BoardPid, ActionsPid}) ->
  BoardInfo = s:s(BoardPid, info),
  ActionsInfo = s:s(ActionsPid, info),
  {reply, #{status => Status, board => BoardInfo,
            actions => ActionsInfo}, State}.

init([]) -> 
  {ok, BoardPid} = board:go(),
  {ok, ActionsPid} = actions:go(),
  {ok, {#{status => playing}, BoardPid, ActionsPid}}.

go() ->
  gen_server:start_link(?MODULE, [], []).

