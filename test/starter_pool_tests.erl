-module(starter_pool_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

evaluate_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   fun(Setup) ->
       {inparallel,
        [ruby_evaluate(Setup),
         javascript_evaluate(Setup)]}
   end}.

start() ->
  starter:start().

stop(_) ->
  starter:stop().

ruby_evaluate(_) ->
  Response = starter_pool:evaluate(ruby, "puts 1; put 1"),
  Stdout = <<"1\n">>,
  Stderr = <<"-e:1:in `<main>': undefined method `put' for main:Object (NoMethodError)\n">>,

  [?_assertEqual([{stderr, Stderr}, {stdout, Stdout}], Response)].

javascript_evaluate(_) ->
  [{stderr, Stderr}, {stdout, Stdout}] = starter_pool:evaluate(javascript, "console.log(1); log()"),
  ExpectedStdout = <<"1\n">>,

  [?_assertEqual(ExpectedStdout, Stdout),
   ?_assertMatch({match, _}, re:run(Stderr, "ReferenceError: log is not defined", [global]))].
