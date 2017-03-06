-module(table_test).
-include_lib("eunit/include/eunit.hrl").

default_board_test() ->
  {ok, Table} = table:go(),
  DefaultBoard = board:default(),
  #{board := DefaultBoard} = s:s(Table, info).

first_turn_action_swap_test() ->
  false = test.