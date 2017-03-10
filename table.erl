-module(table).
-compile(export_all).

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({place, Action, Player, Position}, _, State = {#{status := _}, BoardPid}) ->
% todo: actions test current player
  s:s(BoardPid, {place, Player, {Action, Player, Position}}),
  % todo: check for win, update Status
  {reply, ok, State};  
handle_call(info, _, State = {#{status := Status}, BoardPid}) ->
  BoardInfo = s:s(BoardPid, info),
  {reply, #{status => Status, board => BoardInfo}, State}.

init([]) -> 
  {ok, BoardPid} = board:go(),
  {ok, {#{status => playing}, BoardPid}}.

go() ->
  gen_server:start_link(?MODULE, [], []).

