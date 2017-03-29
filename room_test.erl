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
                       cache := #{status := playing}}} = Tables,
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
  #{status := challenging} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := null} = s:s(Room, {status, #{name => "Tremulous.net"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {_, Tables, _} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Marsifrolg"}}),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Blandline"}}).

invalid_auth_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := _Auth}} = s:s(Room, {join, #{name => "Marsifrolg"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {error, invalid_auth} = s:s(Room, {play, #{name => "Marsifrolg", auth => "badauth"}, useless_move}).

invalid_input_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Marsifrolg"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {error, invalid_input} = s:s(Room, {play, #{name => "Marsifrolg", auth => Auth}, {bad, move}}).

out_of_order_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marsifrolg"}}),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Blandline"}}),
  {error, out_of_order} = s:s(Room, {play, #{name => "Blandline", auth => Auth}, {take, {4,5}}}).
