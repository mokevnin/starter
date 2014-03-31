-module(starter_pool).

-export([evaluate/2]).

evaluate(Lng, Code) ->
  F = fun(Worker) ->
          gen_server:call(Worker, {evaluate, Lng, Code})
      end,
  try poolboy:transaction(default, F)
  catch
    exit:Reason -> {error, Reason}
  end.
