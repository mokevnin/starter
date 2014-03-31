-module(starter).

-export([start/0]).

start() ->
  sync:go(),
  {ok, _} = application:ensure_all_started(starter).
