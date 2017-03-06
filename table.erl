-module(table).
-compile(export_all).

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call(info, _, State = {#{status := Status}, BoardPid}) ->
  BoardInfo = s:s(BoardPid, info),
  {reply, #{status => Status, board => BoardInfo}, State}.

init([]) -> 
  {ok, BoardPid} = board:go(),
  {ok, {#{status => playing}, BoardPid}}.

go() ->
  gen_server:start_link(?MODULE, [], []).

