import Platform
import List exposing (..)
import Basics exposing (..)

main : Platform.Program () () ()
main = assert foldrTest
-- main = assert [consTest, foldrTest, map2Test, map2Test2] []

consTest =
  let
    i = 1 :: [2,3]
  in
    case i of
      [1,2,3] -> 0
      _ -> 1

foldrTest =
  let
    result = foldr (+) 0 [1,2,3]
  in
    case result of
      6 -> 0
      _ -> 1

map2Test =
  let
    result = map2 (+) [1,2,3] [1,2,3,4]
  in
    case result of
      [2,4,6] -> 0
      _ -> 1

map2Test2 =
  let
    result = map2 (,) [1,2,3] ['a','b']
  in
    case result of
      [(1,'a'),(2,'b')] -> 0
      _ -> 1


assert: number -> Platform.Program () () ()
assert i =
  case i of
    0 -> Platform.print "."
    _ -> Platform.print "X"

{-
assert : List number -> List Char -> Platform.Program () () ()
assert assertions results =
  case (assertions, results) of
    ((x::xs), results) ->
      case x of
        0 -> assert xs ('.'::results)
        _ -> assert xs ('X'::results)
    ([], results) -> Platform.print results
-}
