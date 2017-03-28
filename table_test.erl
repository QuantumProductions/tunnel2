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