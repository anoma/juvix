module Juvix.Formatter where

import qualified Data.Text                                                       as T
import           Juvix.Compiler.Concrete.Language
import           Juvix.Compiler.Concrete.Print                                   (ppOutDefault)
import qualified Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping as Scoper
import           Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import           Juvix.Compiler.Pipeline.EntryPoint
import           Juvix.Extra.Paths
import           Juvix.Prelude
import           Juvix.Prelude.Pretty

data FormattedFileInfo = FormattedFileInfo
  { _formattedFileInfoPath         :: Path Abs File,
    _formattedFileInfoContentsAnsi :: NonEmpty AnsiText
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
  FormatResultFail <> _         = FormatResultFail
  _ <> FormatResultFail         = FormatResultFail
  FormatResultNotFormatted <> _ = FormatResultNotFormatted
  _ <> FormatResultNotFormatted = FormatResultNotFormatted
  _ <> _                        = FormatResultOK

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
-- If the file requires formatting then the function returns 'FormatResultNotFormatted'
-- and outputs a FormattedFileInfo containing the formatted contents of the file.
--
-- If the file does not require formatting then the function returns
-- 'FormatResultOK' and nothing is output.
format ::
  forall r.
  Members '[ScopeEff, Files, Output FormattedFileInfo] r =>
  Path Abs File ->
  Sem r FormatResult
format p = do
  originalContents <- readFile' p
  formattedContents <- formatPath p
  formatResultFromContents originalContents formattedContents p

-- | Format a Juvix project.
--
-- Format all files in the Juvix project containing the passed directory.
--
-- If any file requires formatting then the function returns 'FormatResultNotFormatted'
-- This function also outputs a FormattedFileInfo (containing the formatted
-- contents of a file) for every file in the project that requires formatting.
--
-- If all files in the project are already formatted then the function returns
-- 'FormatResultOK' and nothing is output.
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

formatPath :: Member ScopeEff r => Path Abs File -> Sem r (Maybe (NonEmpty AnsiText))
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
  formattedContents <- formatScoperResult res
  formatResultFromContents originalContents formattedContents formatStdinPath

formatResultFromContents ::
  forall r.
  Members '[Output FormattedFileInfo] r =>
  Text ->
  Maybe (NonEmpty AnsiText) ->
  Path Abs File ->
  Sem r FormatResult
formatResultFromContents originalContents mfc filepath =
  case mfc of
    Just formattedContents
      | originalContents /= ansiPlainText formattedContents -> do
          output
            ( FormattedFileInfo
                { _formattedFileInfoPath = filepath,
                  _formattedFileInfoContentsAnsi = formattedContents
                }
            )
          return FormatResultNotFormatted
      | otherwise -> return FormatResultOK
    Nothing ->
      return FormatResultOK

formatScoperResult :: Scoper.ScoperResult -> Sem r (Maybe (NonEmpty AnsiText))
formatScoperResult res = do
  let cs = res ^. Scoper.comments
      formattedModules = run (runReader cs (mapM formatTopModule (res ^. Scoper.resultModules)))
  case res ^. Scoper.mainModule . modulePragmas of
    Just pragmas ->
      case pragmas ^. withLocParam . withSourceValue . pragmasFormat of
        Just PragmaFormat {..}
          | not _pragmaFormat ->
              return Nothing
        _ ->
          return (Just formattedModules)
    Nothing ->
      return (Just formattedModules)
  where
    formatTopModule :: Member (Reader Comments) r => Module 'Scoped 'ModuleTop -> Sem r AnsiText
    formatTopModule m = do
      cs <- ask
      return (ppOutDefault cs m)
