-module(room_test).
-include_lib("eunit/include/eunit.hrl").

join_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  {#{}, #{}, #{name := "Marsifrolg"}} = s:s(Room, debug),
  s:s(Room, {join, #{name => "Blandline"}}),
  {Players, Tables, Challenger} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{"Marsifrolg"  := #{status := playing, table_pid := FirstPid},
   "Blandline"    := #{status := playing, table_pid := FirstPid}} = Players,
  #{FirstPid      := #{seats  := #{x := "Marsifrolg", o := "Blandline"},
                       status := unstarted}} = Tables,
   null = Challenger.

cancel_authorized_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Marsifrolg"}}),
  {#{}, #{}, #{name := "Marsifrolg"}} = s:s(Room, debug),
  s:s(Room, {cancel, #{name => "Marsifrolg"}}),
  {_, _, #{name := "Marsifrolg"}} = s:s(Room, debug),
  s:s(Room, {cancel, #{name => "Marsifrolg", auth => Auth}}),
  {_, _, null} = s:s(Room, debug).

status_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  % Debug = s:s(Room, debug),
  % Debug.
  #{status := challenging} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := null} = s:s(Room, {status, #{name => "Tremulous.net"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {_, Tables, _} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Blandline"}}).
