-module(table_test).
-include_lib("eunit/include/eunit.hrl").

default_board_test() ->
  {ok, Table} = table:go(),
  DefaultBoard = board:default(),
  #{board := DefaultBoard} = s:s(Table, info).

x_takes_2_1_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  #{board := [Row1 | _]} = s:s(Table, info),
  [{spawn, x, none}, {recent, x, none}, _, _, _]  = Row1.

first_turn_action_swap_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  #{actions := Actions} = s:s(Table, info),
  {_, o} = Actions.

out_of_order_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, o, {4, 5}}),
  #{actions := Actions} = s:s(Table, info),
  {_, x} = Actions.

regular_play_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {4, 5}}),
  s:s(Table, {place, take, o, {4, 4}}),
  s:s(Table, {place, take, x, {3, 1}}),
  s:s(Table, {place, take, x, {3, 2}}),
  #{actions := Actions} = s:s(Table, info),
  {_, o} = Actions.

regular_play_2_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {4, 5}}),
  #{actions := Actions} = s:s(Table, info),
  {_, o} = Actions.

bad_move_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {4, 5}}),
  s:s(Table, {place, take, o, {4, 4}}),
  s:s(Table, {place, take, x, {3, 1}}),
  s:s(Table, {place, take, x, {3, 4}}),
  #{actions := Actions} = s:s(Table, info),
  {#{x := {1, _}}, x} = Actions.

slices_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {5, 4}}),
  s:s(Table, {place, take, o, {5, 3}}),
  s:s(Table, {place, take, x, {3, 1}}),
  s:s(Table, {place, take, x, {3, 2}}),
  s:s(Table, {place, take, o, {4, 3}}),
  s:s(Table, {place, take, o, {3, 3}}),
  s:s(Table, {place, take, x, {4, 2}}),
  s:s(Table, {place, take, x, {4, 3}}),
  #{actions := Actions} = s:s(Table, info),
  {#{x := {_, 1}, o := {3, _}}, o} = Actions.

win_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {5, 4}}),
  s:s(Table, {place, take, o, {5, 3}}),
  s:s(Table, {place, take, x, {3, 1}}),
  s:s(Table, {place, take, x, {3, 2}}),
  s:s(Table, {place, take, o, {5, 2}}),
  s:s(Table, {place, take, o, {5, 1}}),
  s:s(Table, {place, take, x, {3, 3}}),
  s:s(Table, {place, take, x, {2, 3}}),
  s:s(Table, {place, take, o, {4, 1}}),
  s:s(Table, {place, take, o, {3, 1}}),
  s:s(Table, {place, take, x, {1, 2}}),
  s:s(Table, {place, take, x, {1, 3}}),
  s:s(Table, {place, take, x, {1, 4}}),
  s:s(Table, {place, take, o, {2, 1}}),
  s:s(Table, {place, take, o, {1, 1}}),
  #{status := Status} = s:s(Table, info),  
  owin = Status.

timeout_x_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {timeout, x}),
  s:s(Table, {timeout, o}),
  #{status := owin} = s:s(Table, info).

timeout_o_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {timeout, o}),
  #{status := xwin} = s:s(Table, info).