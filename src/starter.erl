-module(starter).

-export([start/0, stop/0]).

start() ->
  sync:go(),
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  Apps = [sync, starter, poolboy],
  [application:stop(App) || App <- Apps],
  ok.

