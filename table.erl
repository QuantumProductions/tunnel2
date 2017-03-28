-module(table).
-compile(export_all).

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({place, Action, Player, Position}, _, State = {#{status := _}, BoardPid, ActionsPid}) ->
  % todo: actions test current player
  {ok, _NewBoard, Slices, RecentTaken} = s:s(BoardPid, {place, Player, {Action, Player, Position}}),
  s:s(ActionsPid, {move, Slices, RecentTaken}),
  {reply, ok, State};  
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

