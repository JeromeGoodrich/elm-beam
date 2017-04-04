module Platform exposing (..)

import Native.Test.Console

type Program flags model msg = Program

print : a -> Program () () ()
print i =
  Native.Test.Console.print i
