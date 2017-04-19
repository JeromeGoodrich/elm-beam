{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version
    , parseDependencies, Tag(..)
    , compile, Context(..), Result(..)
    , Localizer, dummyLocalizer
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    )
    where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.ByteString.Builder as BS
import Data.Text (Text)
import System.IO (Handle)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version
import qualified Elm.Docs as Docs
import qualified Elm.Package as Package
import qualified Generate.CoreErlang as Core
import qualified Parse.Helpers as Parse (run)
import qualified Parse.Module as Parse (header)
import qualified Reporting.Annotation as A
import qualified Reporting.Bag as Bag
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- VERSION


version :: Package.Version
version =
  Elm.Compiler.Version.version



-- DEPENDENCIES


data Tag
  = Normal
  | Effect
  | Port


parseDependencies :: Package.Name -> Text -> Either Error (Tag, PublicModule.Raw, [PublicModule.Raw])
parseDependencies pkgName sourceCode =
  case Parse.run Parse.header sourceCode of
    Right header ->
      Right $ getDeps pkgName header

    Left err ->
      Left (Error (A.map Error.Syntax err))


getDeps :: Package.Name -> Module.Header [Module.UserImport] -> (Tag, PublicModule.Raw, [PublicModule.Raw])
getDeps pkgName (Module.Header sourceTag name _ _ _ imports) =
  let
    tag =
      case sourceTag of
        Module.Normal -> Normal
        Module.Port _ -> Port
        Module.Effect _ -> Effect

    deps =
      if pkgName == Package.core then
        map (fst . A.drop) imports
      else
        map (fst . A.drop) imports ++ map fst Imports.defaults
  in
    ( tag, name, deps )



-- COMPILATION


{-| Compiles Elm source code to Core Erlang. -}
compile :: Context -> Text -> PublicModule.Interfaces -> (Localizer, [Warning], Either [Error] Result)
compile context source interfaces =
  let
    (Context packageName isExposed dependencies) =
      context

    (Result.Result oneLocalizer warnings answer) =
      do  modul <- Compile.compile packageName dependencies interfaces source
          docs <- Result.format id (docsGen isExposed modul)

          let interface = Module.toInterface packageName modul
          let allInterfaces = Map.insert (Module.name modul) interface interfaces
          let javascript = {-# SCC elm_compiler_generate #-} Core.generate allInterfaces modul

          return (Result docs interface javascript)
  in
    ( Result.oneToValue dummyLocalizer Localizer oneLocalizer
    , Bag.toList Warning warnings
    , Result.answerToEither Error id answer
    )


data Context =
  Context
    { _packageName :: Package.Name
    , _isExposed :: Bool
    , _dependencies :: [PublicModule.Canonical]
    }


data Result = Result
    { _docs :: Maybe Docs.Documentation
    , _interface :: PublicModule.Interface
    , _core :: BS.Builder
    }


docsGen :: Bool -> Module.Optimized -> Result.Result () w Error.Error (Maybe Docs.Documentation)
docsGen isExposed (Module.Module name _ info) =
  if not isExposed then
    Result.ok Nothing

  else
    let
      toDocs checked =
        Just (Docs.fromCheckedDocs (ModuleName._module name) checked)
    in
      toDocs <$> Docs.check (Module.exports info) (Module.docs info)



-- DEALIASER


newtype Localizer =
    Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
    Localizer Map.empty



-- ERRORS


newtype Error =
    Error (A.Located Error.Error)


errorToString :: Localizer -> String -> Text -> Error -> String
errorToString (Localizer localizer) location source (Error (A.A region err)) =
    Report.toString location region (Error.toReport localizer err) source


printError :: Handle -> Localizer -> String -> Text -> Error -> IO ()
printError handle (Localizer localizer) location source (Error (A.A region err)) =
    Report.toHandle handle location region (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> Error -> Json.Value
errorToJson (Localizer localizer) location (Error err) =
    Error.toJson localizer location err



-- WARNINGS


newtype Warning =
    Warning (A.Located Warning.Warning)


warningToString :: Localizer -> String -> Text -> Warning -> String
warningToString (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toString location region (Warning.toReport localizer wrn) source


printWarning :: Handle -> Localizer -> String -> Text -> Warning -> IO ()
printWarning handle (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toHandle handle location region (Warning.toReport localizer wrn) source


warningToJson :: Localizer -> String -> Warning -> Json.Value
warningToJson (Localizer localizer) location (Warning wrn) =
    Warning.toJson localizer location wrn
