module Juvix.Formatter where

import Data.Text qualified as T
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Paths
import Juvix.Prelude
import Juvix.Prelude.Pretty

data FormattedFileInfo = FormattedFileInfo
  { _formattedFileInfoPath :: Path Abs File,
    _formattedFileInfoContentsAnsi :: NonEmpty AnsiText
  }

data ScopeEff m a where
  ScopeFile :: Path Abs File -> ScopeEff m Scoper.ScoperResult
  ScopeStdin :: ScopeEff m Scoper.ScoperResult

makeLenses ''FormattedFileInfo
makeSem ''ScopeEff

data FormatResult
  = FormatResultOK
  | FormatResultFail
  deriving stock (Eq)

combineResults :: [FormatResult] -> FormatResult
combineResults rs
  | FormatResultFail `elem` rs = FormatResultFail
  | otherwise = FormatResultOK

ansiPlainText :: NonEmpty AnsiText -> Text
ansiPlainText = T.concat . toList . fmap toPlainText

formattedFileInfoContentsText :: SimpleGetter FormattedFileInfo Text
formattedFileInfoContentsText = to infoToPlainText
  where
    infoToPlainText :: FormattedFileInfo -> Text
    infoToPlainText fInfo = ansiPlainText (fInfo ^. formattedFileInfoContentsAnsi)

-- | Format a single Juvix file.
--
-- If the file requires formatting then the function returns FormatResultFail
-- and outputs a FormattedFileInfo containing the formatted contents of the file.
--
-- If the file does not require formatting then the function returns
-- FormatResultOK and nothing is output.
format ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Path Abs File ->
  Sem r FormatResult
format p = do
  originalContents <- readFile' p
  formattedContents <- formatPath p
  if
      | originalContents /= (ansiPlainText formattedContents) -> do
          output
            ( FormattedFileInfo
                { _formattedFileInfoPath = p,
                  _formattedFileInfoContentsAnsi = formattedContents
                }
            )
          return FormatResultFail
      | otherwise -> return FormatResultOK

-- | Format a Juvix project.
--
-- Format all files in the Juvix project containing the passed directory.
--
-- If any file requires formatting then the function returns FormatResultFail.
-- This function also outputs a FormattedFileInfo (containing the formatted
-- contents of a file) for every file in the project that requires formatting.
--
-- If all files in the project are already formatted then the function returns
-- FormatResultOK and nothing is output.
--
-- NB: This function does not traverse into Juvix sub-projects, i.e into
-- subdirectories that contain a juvix.yaml file.
formatProject ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Path Abs Dir ->
  Sem r FormatResult
formatProject p = do
  walkDirRelAccum handler p FormatResultOK
  where
    handler ::
      Path Abs Dir ->
      [Path Rel Dir] ->
      [Path Rel File] ->
      FormatResult ->
      Sem r (FormatResult, Recurse Rel)
    handler cd _ files _ = do
      let juvixFiles = [cd <//> f | f <- files, isJuvixFile f]
      res <- combineResults <$> mapM format juvixFiles
      return (res, RecurseFilter (\hasJuvixYaml d -> not (hasJuvixYaml && isHiddenDirectory d)))

formatPath :: Member ScopeEff r => Path Abs File -> Sem r (NonEmpty AnsiText)
formatPath p = do
  res <- scopeFile p
  formatScoperResult res

formatStdin ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Sem r FormatResult
formatStdin = do
  res <- scopeStdin
  formattedContents <- formatScoperResult res
  output
    ( FormattedFileInfo
        { _formattedFileInfoPath = formatStdinPath,
          _formattedFileInfoContentsAnsi = formattedContents
        }
    )
  return FormatResultFail

formatScoperResult :: Scoper.ScoperResult -> Sem r (NonEmpty AnsiText)
formatScoperResult res = do
  let cs = res ^. Scoper.comments
      formattedModules = run (runReader cs (mapM formatTopModule (res ^. Scoper.resultModules)))
  return formattedModules
  where
    formatTopModule :: Member (Reader Comments) r => Module 'Scoped 'ModuleTop -> Sem r AnsiText
    formatTopModule m = do
      cs <- ask
      return (ppOutDefault cs m)
