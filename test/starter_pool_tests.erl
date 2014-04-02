-module(starter_pool_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

evaluate_test() ->
  starter:start(),
  Response = starter_pool:evaluate(ruby, "puts 1; put 1"),
  Stderr = <<"-e:1:in `<main>': undefined method `put' for main:Object (NoMethodError)\n">>,
  [{stderr, Stderr}, {stdout, <<"1\n">>}] = Response,
  ok.
