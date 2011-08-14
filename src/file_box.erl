-module(file_box).

-export([start/0]).

start() ->
  application:start(?MODULE).
