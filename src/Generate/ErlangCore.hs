{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Module.Name as ModuleName
import qualified AST.Expression.Optimized as Opt
import qualified Generate.ErlangCore.Builder as Core
import qualified Elm.Package as Pkg
import qualified AST.Literal as Literal


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    body =
      map generateDef (Module.program info)
  in
    Core.functionsToText body



generateDef :: Opt.Def -> Core.Function
generateDef def =
  case def of
    Opt.Def (Opt.Facts home) name body ->
        defineFunction home name (generateExpr body)


generateExpr :: Opt.Expr -> Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal literal ->
      generateLiteral literal

    Opt.List exprs ->
      Core.List (map generateExpr exprs)

    Opt.Var var ->
      generateVar var

    Opt.Function args body ->
      let
        fun argName coreExpr =
          Core.Fun argName coreExpr
      in
        foldr fun (generateExpr body) args

    Opt.Call function args ->
      generateCall function args

    Opt.Ctor name exprs ->
      Core.Tuple (Core.Atom name : map generateExpr exprs)


generateLiteral :: Literal.Literal -> Core.Expr
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Float n

    Literal.IntNum n ->
      Core.Int n

    Literal.Chr c ->
      Core.Char c


defineFunction :: Maybe ModuleName.Canonical -> Text -> Core.Expr -> Core.Function
defineFunction maybeHome functionName body =
  let
    name =
      maybe id qualified maybeHome functionName
  in
    Core.Function name body

generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  let
    applyGlobal moduleName =
      Core.Apply (Core.FunctionRef (qualified moduleName name) 0) []
  in
    case home of
      Var.Local ->
        Core.Var name

      Var.Module moduleName ->
        applyGlobal moduleName

      Var.TopLevel moduleName ->
        applyGlobal moduleName


generateCall :: Opt.Expr -> [Opt.Expr] -> Core.Expr
generateCall function args =
  let
    apply coreExpr elmExpr =
      Core.Apply coreExpr [generateExpr elmExpr]
  in
    case function of
      Opt.Var var@(Var.Canonical (Var.Module modul) name) | Var.isNative var ->
        Core.Call (moduleToText modul) name (map generateExpr args)

      _ ->
        foldl apply (generateExpr function) args


moduleToText :: ModuleName.Canonical -> Text
moduleToText (ModuleName.Canonical (Pkg.Name user project) moduleName) =
  let
    safeUser =
      Text.replace "-" "_" user

    safeProject =
      Text.replace "-" "_" project

    safeModuleName =
      Text.replace "." "_" moduleName
  in
    safeUser <> "@" <> safeProject <> "@" <> safeModuleName


qualified :: ModuleName.Canonical -> Text -> Text
qualified moduleName name =
  moduleToText moduleName <> "@" <> name
