-module(langs).

-export([command/2]).

command(Lang, FilePath) ->
  StrLang = to_str(Lang),
  icommand(StrLang, FilePath).

icommand("javascript", FilePath) ->
  lists:concat(["node ", FilePath]);

icommand("erlang", FilePath) ->
  lists:concat(["cat ", FilePath, " | erl"]);

icommand(Lang, FilePath) ->
  lists:concat([Lang, " ", FilePath]).

to_str(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_str(Data) when is_atom(Data) ->
  lists:flatten(io_lib:format("~p", [Data]));
to_str(Data) ->
  Data.

