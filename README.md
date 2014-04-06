### install
    make
### examples
    5> starter_pool:evaluate(python, "print 1;").
    [{stderr,<<>>},{stdout,<<"1\n">>}]

    6> starter_pool:evaluate(ruby, "puts 1; put 1").
    [{stderr,<<"/tmp/starter/6a612664-bd57-11e3-a9c0-0800273b7fe4:1:in `<main>': undefined method `put' for main:Object "...>>},
     {stdout,<<"1\n">>}]

    7> starter_pool:evaluate(javascript, "console.log(1)").
    [{stderr,<<>>},{stdout,<<"1\n">>}]
