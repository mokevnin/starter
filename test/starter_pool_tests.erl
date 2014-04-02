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
         python_evaluate(Setup),
         php_evaluate(Setup),
         erlang_evaluate(Setup),
         javascript_evaluate(Setup)]}
   end}.

start() ->
  starter:start().

stop(_) ->
  starter:stop().

ruby_evaluate(_) ->
  fun() ->
      [{stderr, Stderr}, {stdout, Stdout}] = starter_pool:evaluate(ruby, "puts 1; put 1"),
      ExpectedStdout = <<"1\n">>,
      ExpectedStderr = "undefined method `put' for main:Object",

      ?assertEqual(ExpectedStdout, Stdout),
      ?assertMatch({match, _}, re:run(Stderr, ExpectedStderr, [global]))
  end.

javascript_evaluate(_) ->
  fun() ->
      [{stderr, Stderr}, {stdout, Stdout}] = starter_pool:evaluate(javascript, "console.log(1); log()"),
      ExpectedStdout = <<"1\n">>,
      ExpectedStderr =  "ReferenceError: log is not defined",

      ?assertEqual(ExpectedStdout, Stdout),
      ?assertMatch({match, _}, re:run(Stderr, ExpectedStderr, [global]))
  end.

python_evaluate(_) ->
  fun() ->
      [{stderr, _Stderr}, {stdout, Stdout}] = starter_pool:evaluate(python, "print 1"),
      ExpectedStdout = <<"1\n">>,

      ?assertEqual(ExpectedStdout, Stdout)
  end.

php_evaluate(_) ->
  fun() ->
      [{stderr, _Stderr}, {stdout, Stdout}] = starter_pool:evaluate(php, "<?php print(1); ?>"),
      ExpectedStdout = <<"1">>,

      ?assertEqual(ExpectedStdout, Stdout)
  end.

erlang_evaluate(_) ->
  fun() ->
      [{stderr, _Stderr}, {stdout, Stdout}] = starter_pool:evaluate(erlang, "erlang:display(777)."),

      ?assertMatch({match, _}, re:run(Stdout, "1> 777", [global]))
  end.
