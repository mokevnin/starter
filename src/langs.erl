-module(langs).

-export([command/2]).

command(ruby, Code) ->
  lists:concat(["ruby -e '", Code, "'"]);

command(javascript, Code) ->
  lists:concat(["node -e '", Code, "'"]).

