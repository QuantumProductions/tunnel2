-module(room_test).
-include_lib("eunit/include/eunit.hrl").

join_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  {#{}, #{}, "Marsifrolg"} = s:s(Room, debug),
  s:s(Room, {join, #{name => "Blandline"}}),
  {Players, Tables, Challenger} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{"Marsifrolg"  := #{status := playing, table_pid := FirstPid},
   "Blandline"    := #{status := playing, table_pid := FirstPid}} = Players,
  #{FirstPid      := #{seats  := #{x := "Marsifrolg", o := "Blandline"},
                       status := unstarted}} = Tables,
   null = Challenger.

cancel_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  {#{}, #{}, "Marsifrolg"} = s:s(Room, debug),
  s:s(Room, {cancel, #{name => "Marsifrolg"}}),
  {_, _, Challenger} = s:s(Room, debug),
  null = Challenger.

status_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  #{status := challenging} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := null} = s:s(Room, {status, #{name => "Tremulous.net"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {_, Tables, _} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Blandline"}}).

  % s:s(Room, {join, #{name => "Blandline"}}),