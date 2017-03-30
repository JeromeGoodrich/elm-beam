module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy.IO as LazyTextIO
import qualified System.Environment as System
import qualified Data.Text.Lazy as LazyText
import Data.Text (Text)
import System.FilePath.Find (find, (==?), extension)
import System.Exit (exitFailure)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified AST.Expression.Canonical as Can
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize
import qualified Elm.Package as Package
import qualified Nitpick.PatternMatches as Nitpick
import qualified Nitpick.TopLevelTypes as Nitpick
import qualified Parse.Parse as Parse (program)
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Inference as TI

import qualified Bootstrap.Erlang as Erl


-- COMPILE


type Result =
  Result.Result (Result.One RenderType.Localizer) Warning.Warning Error.Error


newtype Error =
    Error (A.Located Error.Error)


dummyLocalizer :: RenderType.Localizer
dummyLocalizer =
    Map.empty


compile
    :: Package.Name
    -> Map.Map ModuleName.Raw ModuleName.Canonical
    -> Module.Interfaces
    -> Text
    -> Result LazyText.Text
compile packageName importDict interfaces source =
  do
      -- Parse the source code
      validModule <-
          Result.format Error.Syntax $
            Parse.program packageName source

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <-
          Canonicalize.module' importDict interfaces validModule

      -- Run type inference on the program.
      types <-
          Result.from Error.Type $
            TI.infer interfaces canonicalModule

      -- One last round of checks
      canonicalDefs <-
          Result.format Error.Type $
            Nitpick.topLevelTypes types $
              Can.toSortedDefs (Module.program (Module.info canonicalModule))

      _tagDict <-
        Result.format Error.Pattern $
          Nitpick.patternMatches interfaces canonicalModule

      return $ Erl.defsToText canonicalDefs


errorToString :: Text -> Error -> String
errorToString code (Error (A.A region error)) =
  Report.nonAnsiRender $
    Report.toDoc "" region (Error.toReport dummyLocalizer error) code


make :: FilePath -> IO ()
make file =
  do  code <-
        TextIO.readFile file

      let (Result.Result _oneLocalizer _warnings answer) =
            compile Package.core Map.empty Map.empty code

      case Result.answerToEither Error id answer of
        Right erlx ->
          LazyTextIO.writeFile (file ++ ".erlx") erlx

        Left errors ->
          do  let messages = concatMap (errorToString code) errors
              putStrLn $ "Compile failed:\n" ++ messages
              exitFailure


main :: IO ()
main =
  do  dir <-
        head <$> System.getArgs

      sources <-
        find (return True) (extension ==? ".elm") dir

      mapM_ make sources
