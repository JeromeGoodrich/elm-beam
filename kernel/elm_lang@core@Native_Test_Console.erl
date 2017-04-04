-module(elm_lang@core@Native_Test_Console).

-export([print/1]).

print(Something) ->
  io:fwrite("~p", [Something]).
