-module(table).
-compile(export_all).

init([]) -> 
  {ok, []}.

go() ->
  gen_server:start_link(?MODULE, [], []).
