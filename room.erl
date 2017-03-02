-module(room).
-compile(export_all).

join({Players, Tables, null}, #{name := ChallengerName}) ->
  {ok, {Players, Tables, ChallengerName}};
join({Players, Tables, ContenderName}, #{name := ChallengerName}) ->
  {ok, TablePid} = table:go(),
  Players2 = maps:put(ContenderName,  #{status => playing, table_pid => TablePid},
    Players),
  Players3 = maps:put(ChallengerName, #{status => playing, table_pid => TablePid},
    Players2),
  Tables2 = maps:put(TablePid, #{seats => #{x => ContenderName, o => ChallengerName},
                                 status => unstarted}, Tables),
  {ok, {Players3, Tables2, null}}.

cancel({Players, Tables, ChallengerName}, #{name := ChallengerName}) ->
  {ok, {Players, Tables, null}};
cancel(State, _) ->
  {ok, State}.

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({cancel, PlayerData}, _, State) ->
  {Response, State2} = cancel(State, PlayerData),
  {reply, Response, State2};
handle_call({join, PlayerData}, _, State) ->
  {Response, State2} = join(State, PlayerData),
  {reply, Response, State2}.

init([]) -> 
  Players = #{},
  Tables = #{},
  Challenger = null,
  {ok, {Players, Tables, Challenger}}.

go() ->
  gen_server:start_link(?MODULE, [], []).

