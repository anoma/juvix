module Juvix.Formatter where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Prelude
import Juvix.Prelude.Pretty

data FormattedFileInfo = FormattedFileInfo
  { _formattedFileInfoPath :: Path Abs File,
    _formattedFileInfoContentsAnsi :: NonEmpty AnsiText,
    _formattedFileInfoContentsModified :: Bool
  }

data ScopeEff m a where
  ScopeFile :: Path Abs File -> ScopeEff m Scoper.ScoperResult
  ScopeStdin :: ScopeEff m Scoper.ScoperResult

makeLenses ''FormattedFileInfo
makeSem ''ScopeEff

data FormatResult
  = FormatResultOK
  | FormatResultNotFormatted
  | FormatResultFail
  deriving stock (Eq)

instance Semigroup FormatResult where
  FormatResultFail <> _ = FormatResultFail
  _ <> FormatResultFail = FormatResultFail
  FormatResultNotFormatted <> _ = FormatResultNotFormatted
  _ <> FormatResultNotFormatted = FormatResultNotFormatted
  _ <> _ = FormatResultOK

instance Monoid FormatResult where
  mempty = FormatResultOK

combineResults :: [FormatResult] -> FormatResult
combineResults = mconcat

ansiPlainText :: NonEmpty AnsiText -> Text
ansiPlainText = T.concat . toList . fmap toPlainText

formattedFileInfoContentsText :: SimpleGetter FormattedFileInfo Text
formattedFileInfoContentsText = to infoToPlainText
  where
    infoToPlainText :: FormattedFileInfo -> Text
    infoToPlainText fInfo = ansiPlainText (fInfo ^. formattedFileInfoContentsAnsi)

-- | Format a single Juvix file.
--
-- If the file requires formatting then the function returns 'FormatResultNotFormatted'.
--
-- If the file does not require formatting then the function returns 'FormatResultOK'.
--
-- The function also outputs a FormattedFileInfo containing the formatted
-- contents of the file.
format ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Path Abs File ->
  Sem r FormatResult
format p = do
  originalContents <- readFile' p
  runReader originalContents $ do
    formattedContents <- formatPath p
    formatResultFromContents formattedContents p

-- | Format a Juvix project.
--
-- Format all files in the Juvix project containing the passed directory.
--
-- If any file requires formatting then the function returns
-- 'FormatResultNotFormatted'
--
-- If all files in the project are already formatted then the function returns
-- 'FormatResultOK'.
--
-- This function also outputs a FormattedFileInfo (containing the formatted
-- contents of a file) for every processed file.
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
      return (res, RecurseFilter (\hasJuvixYaml d -> not hasJuvixYaml && not (isHiddenDirectory d)))

formatPath :: Members [Reader Text, ScopeEff] r => Path Abs File -> Sem r (NonEmpty AnsiText)
formatPath p = do
  res <- scopeFile p
  formatScoperResult res

formatStdin ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Sem r FormatResult
formatStdin = do
  res <- scopeStdin
  let originalContents = fromMaybe "" (res ^. Scoper.resultParserResult . resultEntry . entryPointStdin)
  runReader originalContents $ do
    formattedContents <- formatScoperResult res
    formatResultFromContents formattedContents formatStdinPath

formatResultFromContents ::
  forall r.
  Members '[Reader Text, Output FormattedFileInfo] r =>
  NonEmpty AnsiText ->
  Path Abs File ->
  Sem r FormatResult
formatResultFromContents formattedContents filepath = do
  originalContents <- ask
  if
      | originalContents /= ansiPlainText formattedContents -> mkResult FormatResultNotFormatted
      | otherwise -> mkResult FormatResultOK
  where
    mkResult :: FormatResult -> Sem r FormatResult
    mkResult res = do
      output
        ( FormattedFileInfo
            { _formattedFileInfoPath = filepath,
              _formattedFileInfoContentsAnsi = formattedContents,
              _formattedFileInfoContentsModified = res == FormatResultNotFormatted
            }
        )
      return res

formatScoperResult :: Member (Reader Text) r => Scoper.ScoperResult -> Sem r (NonEmpty AnsiText)
formatScoperResult res = do
  let cs = res ^. Scoper.comments
      formattedModules = run (runReader cs (mapM formatTopModule (res ^. Scoper.resultModules)))
  case res ^. Scoper.mainModule . modulePragmas of
    Just pragmas ->
      case pragmas ^. withLocParam . withSourceValue . pragmasFormat of
        Just PragmaFormat {..}
          | not _pragmaFormat ->
              NonEmpty.singleton . mkAnsiText @Text <$> ask
        _ ->
          return formattedModules
    Nothing ->
      return formattedModules
  where
    formatTopModule :: Member (Reader Comments) r => Module 'Scoped 'ModuleTop -> Sem r AnsiText
    formatTopModule m = do
      cs <- ask
      return (ppOutDefault cs m)
