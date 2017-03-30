{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Bootstrap.Erlang (defsToText) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Data.Text.Lazy as LazyText
import qualified Data.List as List

import qualified AST.Expression.Canonical as Can
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Package
import qualified Reporting.Annotation as A


defsToText :: [Can.Def] -> LazyText.Text
defsToText defs =
  toLazyText $ list (map fromDef defs) <> "."


fromDef :: Can.Def -> Builder
fromDef (Can.Def _region pattern body _type) =
  ctor "Def" [ fromPattern pattern, fromExpr body ]


fromMain :: Can.Main -> Builder
fromMain main =
  case main of
    Can.VDom ->
      ctor "VDom" []

    Can.NoFlags ->
      ctor "NoFlags" []

    Can.Flags _type ->
      ctor "Flags" []


fromExpr :: Can.Expr -> Builder
fromExpr expr =
  case A.drop expr of
    Can.Literal literal ->
      ctor "ELit" [ fromLiteral literal ]

    Can.Var var ->
      ctor "EVar" [ fromVar var ]

    Can.List exprs ->
      ctor "List" [ list (map fromExpr exprs) ]

    Can.Binop var lhs rhs ->
      ctor "Binop" [ fromVar var, fromExpr lhs, fromExpr rhs ]

    Can.Lambda pattern body ->
      ctor "Lambda" [ fromPattern pattern, fromExpr body ]

    Can.App f arg ->
      ctor "App" [ fromExpr f, fromExpr arg ]

    Can.If branches else_ ->
      ctor "If" [ fromBindings fromExpr branches , fromExpr else_ ]

    Can.Let bindings context ->
      ctor "Let" [ list (map fromDef bindings), fromExpr context ]

    Can.Case switch clauses ->
      ctor "Case" [ fromExpr switch, fromBindings fromPattern clauses ]

    Can.Ctor name args ->
      ctor "ECtor" [ fromVar name, list (map fromExpr args) ]

    Can.Access record field ->
      ctor "Access" [ fromExpr record, bitstring field ]

    Can.Update record fields ->
      ctor "Update" [ fromExpr record, fromBindings bitstring fields ]

    Can.Record fields ->
      ctor "ERecord" [ fromBindings bitstring fields ]

    Can.Cmd _moduleName ->
      ctor "Cmd" []

    Can.Sub _moduleName ->
      ctor "Sub" []

    Can.OutgoingPort _name _type ->
      ctor "OutgoingPort" []

    Can.IncomingPort _name _type ->
      ctor "IncomingPort" []

    Can.Program main expr ->
      ctor "Program" [ fromMain main, fromExpr expr ]

    Can.SaveEnv _moduleName _effects ->
      ctor "SaveEnv" []

    Can.GLShader _ _ _ ->
      ctor "GLShader" []


fromPattern :: Pattern.Canonical -> Builder
fromPattern pattern =
  case A.drop pattern of
    Pattern.Ctor var patterns ->
      ctor "PCtor" [ list (map fromPattern patterns) ]

    Pattern.Record fields ->
      ctor "PRecord" [ list (map bitstring fields) ]

    Pattern.Alias name pattern ->
      ctor "Alias" [ bitstring name, fromPattern pattern ]

    Pattern.Var name ->
      ctor "PVar" [ bitstring name ]

    Pattern.Anything ->
      ctor "Anything" []

    Pattern.Literal literal ->
      ctor "PLit" [ fromLiteral literal ]


fromVar :: Var.Canonical -> Builder
fromVar (Var.Canonical home name) =
  ctor "Variable" [ fromHome home, bitstring name ]


fromModuleName :: ModuleName.Canonical -> Builder
fromModuleName (ModuleName.Canonical (Package.Name user project) modul) =
  ctor "ModuleName" [ bitstring user, bitstring project , bitstring modul ]


fromHome :: Var.Home -> Builder
fromHome home =
  case home of
    Var.BuiltIn ->
      ctor "BuiltIn" []

    Var.Module moduleName ->
      ctor "Module" [ fromModuleName moduleName ]

    Var.TopLevel moduleName ->
      ctor "TopLevel" [ fromModuleName moduleName ]

    Var.Local ->
      ctor "Local" []


fromLiteral :: Literal.Literal -> Builder
fromLiteral literal =
  case literal of
    Literal.Chr text ->
      ctor "Chr" [ bitstring text ]

    Literal.Str text ->
      ctor "Str" [ bitstring text ]

    Literal.IntNum int ->
      ctor "IntNum" [ decimal int ]

    Literal.FloatNum double ->
      ctor "FloatNum" [ realFloat double ]

    Literal.Boolean bool ->
      ctor "Boolean" [ if bool then atom "true" else atom "false" ]



-- HELPERS


fromBindings :: (a -> Builder) -> [(a, Can.Expr)] -> Builder
fromBindings buildLeft =
  let
    build (left, right) =
      tuple [ buildLeft left, fromExpr right ]
  in
    list . map build


ctor :: Text -> [Builder] -> Builder
ctor name args =
  tuple (atom name : args)


atom :: Text -> Builder
atom text =
  "'" <> fromText text <> "'"

tuple :: [Builder] -> Builder
tuple builders =
  "{" <> commaSep builders <> "}"


list :: [Builder] -> Builder
list builders =
  "[" <> commaSep builders <> "]"


commaSep :: [Builder] -> Builder
commaSep =
  mconcat . List.intersperse ","


bitstring :: Text -> Builder
bitstring value =
  "<<\"" <> fromText value <> "\">>"
