-module(room).
-compile(export_all).

randomString(Len) ->
    Chars = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"),
    CharsSize = size(Chars),
    F = fun(_, R) -> [element(rand:uniform(CharsSize), Chars) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

generateAuth() -> binary:list_to_bin(randomString(20)).

join({Players, Tables, null}, #{name := ChallengerName}) ->
  PlayerData = #{name => ChallengerName, auth => generateAuth()},
  {{ok, PlayerData}, {Players, Tables, PlayerData}};
join({Players, Tables, #{name := ContenderName, auth := ContenderAuth}}, #{name := ChallengerName}) ->
  {ok, TablePid} = table:go(),
  Players2 = maps:put(ContenderName,  #{auth => ContenderAuth, status => playing, table_pid => TablePid},
    Players),
  ChallengerAuth = generateAuth(),
  Players3 = maps:put(ChallengerName, #{auth => ChallengerAuth, status => playing, table_pid => TablePid},
    Players2),
  TableCache = s:s(TablePid, info),
  Tables2 = maps:put(TablePid, #{seats => #{x => ContenderName, o => ChallengerName},
                                 cache => TableCache}, Tables),
  {{ok, #{name => ChallengerName, auth => ChallengerAuth}}, {Players3, Tables2, null}}.

validAuth(Players, Name, Auth) ->
  case maps:is_key(Name, Players) of
    true -> case maps:get(Name, Players) of
      Auth -> true;
      Foo -> Foo
    end;
    false -> false
  end.

cancel(State = {_Players, _Tables, null}, _) ->
  {{error, bad_sequence}, State};
cancel(State, Canceller) ->
  cancel(matches, State, Canceller).
cancel(matches, State = {_Players, _Tables, #{name := ContenderName}}, Challenger = #{name := ChallengerName}) ->
  case ContenderName of
    ChallengerName -> cancel(validate, State, Challenger);
    _ ->  {{error, bad_sequence}, State}
  end;
cancel(validate, State = {Players, Tables, #{auth := ContenderAuth}}, #{auth := Auth}) ->
  case ContenderAuth == Auth of
    true -> {ok, {Players, Tables, null}};
    false -> {{error, invalid_auth}, State}
  end;
cancel(validate, State, _) ->
  {{error, invalid_auth}, State}.

status(Triple, #{name := Name}) ->
  case Triple of
    {_Players, _Tables, #{name :=Name}} -> #{status => challenging};
    {#{Name := PlayerStatus}, _, _} -> PlayerStatus;
    _ -> #{status => null}
  end.

valid_auth(Players, Name, Auth) ->
  #{auth := ExistingAuth, table_pid := TablePid} = maps:get(Name, Players),
  case ExistingAuth == Auth of
    true -> {true, TablePid};
    false -> false
  end.

play(valid_table, Tables, TablePid, Name, {Action, Position}) ->
  #{seats := #{x := XName, o := OName}} = maps:get(TablePid, Tables),
  case Name of
    XName -> s:s(TablePid, {place, Action, x, Position});
    OName -> s:s(TablePid, {place, Action, o, Position});
    _ -> {error, bad_team}
  end.

play(State = {Players, Tables, _Challenger}, #{name := Name, auth := Auth}, Move) ->
  case maps:is_key(Name, Players) of
    true ->
      case valid_auth(Players, Name, Auth) of
        {true, TablePid} -> 
          case maps:is_key(TablePid, Tables) of
            true ->  {play(valid_table, Tables, TablePid, Name, Move), State};
            false -> {{error, invalid_table}, State}
          end;
        false -> {{error, invalid_auth}, State}
      end;
    false -> {{error, invalid_player}, State}
  end.

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({status, PlayerData}, _, State) ->
  R = status(State, PlayerData),
  {reply, R, State};
handle_call({cancel, PlayerData}, _, State) ->
  {Response, State2} = cancel(State, PlayerData),
  {reply, Response, State2};
handle_call({join, PlayerData}, _, State) ->
  {Response, State2} = join(State, PlayerData),
  {reply, Response, State2};
handle_call({play, Player, Move}, _, State) ->
  {Response, State2} = play(State, Player, Move),
  {reply, Response, State2};
handle_call({update, _Delta}, _, State) ->
  State2 = update(tables, State),
  {reply, State2, State2}.

init([]) -> 
  Players = #{},
  Tables = #{},
  Challenger = null,
  {ok, {Players, Tables, Challenger}}.

go() ->
  {ok, Self} = gen_server:start_link(?MODULE, [], []),
  {ok, _Tref} = timer:apply_after(1000, ?MODULE, update, [start, Self, erlang:timestamp()]),
  {ok, Self}.

% gameFinished(#{status := playing}) -> false;
% gameFinished(_) -> true.

shouldRemoveTable(_) -> false.

removedTableData(TableData, Pid) ->
  case maps:is_key(Pid, TableData) of
    true ->
      {_, Remaining} = maps:take(Pid, TableData),
      Remaining;
    false ->
      TableData
  end.

update(tables, {Players, Tables, Challenger}) ->
  {Players2, Tables2} = update(Tables, Players),
  {Players2, Tables2, Challenger};
update(Tables, Players) ->
  update(maps:keys(Tables), Tables, Players, #{}).

update(start, Self, Then) ->
  Delta = timer:now_diff(erlang:timestamp(), Then) / 10000,
  {ok, _Tref} = timer:apply_after(1000, ?MODULE, update, [start, Self, erlang:timestamp()]),
  s:s(Self, {update, Delta}).

update([], _, Players, Cache) ->
  {Players, Cache};
update([ HPid | T], TableData, Players, Cache) ->
  TableStatus = s:s(HPid, info),
  % case gameFinished(TableStatus) of
  case shouldRemoveTable(TableStatus) of
    true ->
      TableData2 = removedTableData(TableData, HPid),
      update(T, TableData2, Players, Cache);
    false ->
      SingleTableCache = s:s(HPid, info),
      Cache2 = maps:put(HPid, SingleTableCache, Cache),
      update(T, TableData, Players, Cache2)
  end.

