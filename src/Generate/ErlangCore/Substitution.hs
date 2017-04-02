module Generate.ErlangCore.Substitution
  ( one, two, many
  , fresh
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.ErlangCore.Builder as Core



-- COLLECTOR

{-| Keeps track of the variables that we want to bind with `let`.
 -  This lets us think in terms of just the thing we're accumulating,
 -  and bind all of the values afterwards.
 -}

type Collector a
  = State.State Int (Core.Expr -> Core.Expr, a)


(|>) :: Collector a -> (a -> Core.Expr) -> State.State Int Core.Expr
(|>) state toExpr =
  do  (use, a) <- state
      return (use (toExpr a))


substitute :: Core.Expr -> (Core.Constant -> a) -> Collector a
substitute value use =
  case value of
    Core.C constant ->
      return (id, use constant)

    _ ->
      do  name <- fresh
          return (Core.Let name value, use (Core.Var name))



-- PUBLIC


one :: (Core.Constant -> Core.Expr) -> Core.Expr -> State.State Int Core.Expr
one use expr =
  substitute expr use |> id


two
  :: (Core.Constant -> Core.Constant -> Core.Expr)
  -> Core.Expr
  -> Core.Expr
  -> State.State Int Core.Expr
two use first second =
  do  (firstUse, firstC) <-
        substitute first id

      (secondUse, secondC) <-
        substitute second id

      return $ firstUse (secondUse (use firstC secondC))


many
  :: ([Core.Constant] -> Core.Expr)
  -> [Core.Expr]
  -> State.State Int Core.Expr
many use exprs =
  let
    fold (outerUse, oldValue) next =
      do  (innerUse, value) <- substitute next (: oldValue)
          return (innerUse . outerUse, value)
  in
    State.foldM fold (id, []) (reverse exprs) |> use



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
