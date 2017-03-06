-module(table).
-compile(export_all).

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call(info, _, State) ->
  {reply, State, State}.

init([]) -> 
  {ok, #{status => unstarted}}.

go() ->
  gen_server:start_link(?MODULE, [], []).

