{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..), Constant(..), Clause(..)
  , Function(..)
  , functionsToText
  )
  where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(..))
import qualified Data.Text.Lazy as LazyText
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Char as Char



-- EXPRESSIONS


data Expr
  = C Constant
  | Apply Bool Text [Constant] -- apply 'f'/0 ()
  | Call Text Text [Constant] -- call 'module':'f' ()
  | Case Expr [Clause] -- case <_cor0> of ...
  | Let Text Expr Expr -- let <_cor0> = 23 in ...
  | Fun [Text] Expr -- fun () -> ...


data Constant
  = Int Int
  | Char Char
  | Float Double
  | Atom Text
  | Var Text
  | Anything
  | BitString ByteString
  | Tuple [Constant]
  | Cons Constant Constant
  | Nil


data Clause
  = Clause
    { _pattern :: Constant
    , _guard :: Expr
    , _body :: Expr
    }



-- TOP LEVEL


data Function
  = Function Text [Text] Expr -- 'f'/0 = fun () -> ...


functionsToText :: [Function] -> LazyText.Text
functionsToText functions =
  toLazyText (mconcat (map fromFunction functions))


fromFunction :: Function -> Builder
fromFunction function =
  case function of
    Function name args body ->
      fromFunctionName name args <> " = "
      <> fromFun args "" body <> "\n"



-- EXPRESSIONS


fromExpr :: Builder -> Expr -> Builder
fromExpr indent expression =
  case expression of
    C constant ->
      fromConstant constant

    Apply isVariable name args ->
      let
        f =
          if isVariable then
            safeVar name
          else
            fromFunctionName name args
      in
        "apply " <> f <> " (" <> commaSep fromConstant args <> ")"

    Call moduleName functionName args ->
      "call " <> quoted moduleName <> ":" <> quoted functionName <> " ("
      <> commaSep fromConstant args
      <> ")"

    Case expr clauses ->
      let
        clause (Clause pattern guard body) =
          "\n" <> deeper indent <> "<" <> fromConstant pattern
          <> "> when " <> fromExpr indent guard <> " ->\n"
          <> deeper (deeper indent) <> fromExpr (deeper (deeper indent)) body
      in
        "case " <> fromExpr indent expr <> " of"
        <> mconcat (map clause clauses)
        <> "\n" <> indent <> "end"

    Let var binding body ->
      "let <" <> safeVar var <> "> =\n"
      <> deeper indent <> fromExpr (deeper indent) binding <> "\n"
      <> indent <> "in\n"
      <> deeper indent <> fromExpr (deeper indent) body

    Fun args body ->
      fromFun args indent body


fromConstant :: Constant -> Builder
fromConstant constant =
  case constant of
    Int n ->
      decimal n

    Char c ->
      decimal (Char.ord c)

    Float n ->
      formatRealFloat Exponent (Just 20) n

    Atom name ->
      quoted name

    Var name ->
      safeVar name

    Anything ->
      "_"

    BitString str ->
      let
        collectWord c rest =
          "#<" <> fromString (show c)
          <> ">(8,1,'integer',['unsigned'|['big']])" : rest
      in
        "#{" <> commaSep id (ByteString.foldr collectWord [] str) <> "}#"

    Tuple inners ->
      "{" <> commaSep fromConstant inners <> "}"

    Cons first rest ->
      "[" <> fromConstant first <> "|" <> fromConstant rest <> "]"

    Nil ->
      "[]"


fromFunctionName :: Text -> [a] -> Builder
fromFunctionName name args =
  quoted name <> "/" <> decimal (length args)


fromFun :: [Text] -> Builder -> Expr -> Builder
fromFun args indent body =
  "fun (" <> commaSep safeVar args <> ") ->\n"
  <> deeper indent <> fromExpr (deeper indent) body



-- HELPERS


commaSep :: (a -> Builder) -> [a] -> Builder
commaSep toBuilder as =
  mconcat (List.intersperse ", " (map toBuilder as))


safeVar :: Text -> Builder
safeVar name =
  "_" <> fromText name


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"


deeper :: Builder -> Builder
deeper indent =
  indent <> "\t"
