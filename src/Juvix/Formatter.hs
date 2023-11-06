module Juvix.Formatter where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (docDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths
import Juvix.Prelude

data FormattedFileInfo = FormattedFileInfo
  { _formattedFileInfoPath :: Path Abs File,
    _formattedFileInfoContents :: Text,
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
  (Members '[ScopeEff, Files, Output FormattedFileInfo] r) =>
  Path Abs File ->
  Sem r FormatResult
format p = do
  originalContents <- readFile' p
  runReader originalContents $ do
    formattedContents :: Text <- formatPath p
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
  (Members '[ScopeEff, Files, Output FormattedFileInfo] r) =>
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
    handler cd _ files res = do
      let juvixFiles = [cd <//> f | f <- files, isJuvixFile f]
      subRes <- mconcat <$> mapM format juvixFiles
      return (res <> subRes, RecurseFilter (\hasJuvixPackage d -> not hasJuvixPackage && not (isHiddenDirectory d)))

formatPath ::
  (Members '[Reader Text, ScopeEff] r) =>
  Path Abs File ->
  Sem r Text
formatPath p = do
  res <- scopeFile p
  formatScoperResult False res

formatStdin ::
  forall r.
  (Members '[ScopeEff, Files, Output FormattedFileInfo] r) =>
  Sem r FormatResult
formatStdin = do
  res <- scopeStdin
  let originalContents = fromMaybe "" (res ^. Scoper.resultParserResult . resultEntry . entryPointStdin)
  runReader originalContents $ do
    formattedContents :: Text <- formatScoperResult False res
    formatResultFromContents formattedContents formatStdinPath

formatResultFromContents ::
  forall r.
  (Members '[Reader Text, Output FormattedFileInfo] r) =>
  Text ->
  Path Abs File ->
  Sem r FormatResult
formatResultFromContents formattedContents filepath = do
  originalContents <- ask
  if
      | originalContents /= formattedContents -> mkResult FormatResultNotFormatted
      | otherwise -> mkResult FormatResultOK
  where
    mkResult :: FormatResult -> Sem r FormatResult
    mkResult res = do
      output
        ( FormattedFileInfo
            { _formattedFileInfoPath = filepath,
              _formattedFileInfoContents = formattedContents,
              _formattedFileInfoContentsModified = res == FormatResultNotFormatted
            }
        )
      return res

formatScoperResult' ::
  Bool -> Text -> Scoper.ScoperResult -> Text
formatScoperResult' force original sres =
  run . runReader original $ formatScoperResult force sres

formatScoperResult ::
  (Members '[Reader Text] r) =>
  Bool ->
  Scoper.ScoperResult ->
  Sem r Text
formatScoperResult force res = do
  let cs = res ^. Scoper.comments
  formattedModules <-
    runReader cs
      . mapM formatTopModule
      $ res
        ^. Scoper.resultModules
  let txt :: Text = toPlainTextTrim . mconcat . NonEmpty.toList $ formattedModules

  case res ^. Scoper.mainModule . modulePragmas of
    Just pragmas ->
      case pragmas ^. withLocParam . withSourceValue . pragmasFormat of
        Just PragmaFormat {..}
          | not _pragmaFormat && not force -> ask @Text
        _ ->
          return txt
    Nothing ->
      return txt
  where
    formatTopModule :: (Members '[Reader Comments] r) => Module 'Scoped 'ModuleTop -> Sem r (Doc Ann)
    formatTopModule m = do
      cs :: Comments <- ask
      return $ docDefault cs m
