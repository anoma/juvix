{-# LANGUAGE QuasiQuotes #-}

module Commands.Format where

import Commands.Base
import Commands.Format.Options
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Prelude.Pretty (toPlainText)

runCommand :: forall r. Members '[Embed IO, App] r => FormatOptions -> Sem r ()
runCommand opts = case opts ^. formatInputFile of
  Nothing -> error "not implemented"
  Just p -> unlessM (runFilesIO (isFileFormatted p)) (err p)
  where
    err :: AppPath File -> Sem r ()
    err p = exitMsg (ExitFailure 1) [i|File #{p} is not formatted|]

formatAbsFile :: Member App r => Path Abs File -> Sem r Text
formatAbsFile p = formatAppFile appFile
  where
    appFile :: AppPath File
    appFile =
      AppPath
        { _pathPath = Abs p,
          _pathIsInput = True
        }

isFileFormatted :: Members '[Files, App] r => AppPath File -> Sem r Bool
isFileFormatted appFile = do
  p <- someBaseToAbs' (appFile ^. pathPath)
  actual <- readFile' p
  expected <- formatAppFile appFile
  return (actual == expected)

formatAppFile :: Member App r => AppPath File -> Sem r Text
formatAppFile appFile = do
  res <- runPipeline appFile upToScoping
  let cs = res ^. Scoper.comments
      formattedModules = run (runReader cs (mapM formatTopModule (res ^. Scoper.resultModules)))
  return (T.concat (toList formattedModules))
  where
    formatTopModule :: Member (Reader Comments) r => Module 'Scoped 'ModuleTop -> Sem r Text
    formatTopModule m = do
      cs <- ask
      return (toPlainText (ppOutDefault cs m))
