-module(table).
-compile(export_all).

processResult(State = {#{status := playing}, _BoardPid, ActionsPid}, {ok, NewBoard, Slices, RecentTaken}) ->
  s:s(ActionsPid, {move, Slices, RecentTaken}),
  {{ok, NewBoard}, State};
processResult({#{status := playing}, BoardPid, ActionsPid}, {Win, NewBoard, Slices, RecentTaken}) ->
  s:s(ActionsPid, {move, Slices, RecentTaken}),
  {{Win, NewBoard}, {#{status => Win}, BoardPid, ActionsPid}};
processResult(State, {error, Error, _Board}) ->
  {{error, Error}, State}.

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({place, Action, Player, Position}, _, State = {#{status := _}, BoardPid, ActionsPid}) ->
  {_, CurrentPlayer, _, _} = s:s(ActionsPid, info),
  Result = s:s(BoardPid, {place, CurrentPlayer, {Action, Player, Position}}),
  {Response, State2} = processResult(State, Result),
  {reply, Response, State2};    
handle_call(info, _, State = {Status, BoardPid, ActionsPid}) ->
  % todo: update overtime with delta
  BoardInfo = s:s(BoardPid, info),
  {Players, Current, _, _} = s:s(ActionsPid, info),
  ActionsInfo = {Players, Current},
  {reply, #{status => Status, board => BoardInfo,
            actions => ActionsInfo}, State};
handle_call({assign_actions, ActionsPid}, _, {Status, BoardPid}) ->
  State2 = {Status, BoardPid, ActionsPid},
  {reply, State2, State2};
  % ask mailing list about this pattern?
handle_call({timeout, TimeoutPlayer}, _, {#{status := playing}, BoardPid, ActionsPid}) ->
  WinStatus = case TimeoutPlayer of
    x -> owin;
    o -> xwin
  end,
  State2 = {#{status => WinStatus}, BoardPid, ActionsPid},
  {reply, State2, State2};
handle_call(over, _, {_, BoardPid, ActionsPid}) ->
  {noreply, {#{status => over}, BoardPid, ActionsPid}};
handle_call(_, _, State) ->
  {reply, State, State}.

init([]) -> 
  {ok, BoardPid} = board:go(),
  {ok, {#{status => playing}, BoardPid}}.

go() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  {ok, ActionsPid} = actions:go(Pid),
  s:s(Pid, {assign_actions, ActionsPid}),
  {ok, Pid}.
