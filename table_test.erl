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
  % Row1.
  [{spawn, x, none}, {recent, x, none}, _, _, _]  = Row1.

% first_turn_action_swap_test() ->
%   false = test.