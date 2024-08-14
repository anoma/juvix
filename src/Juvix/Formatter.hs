{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Formatter where

import Juvix.Compiler.Concrete.Data.Highlight.Builder (ignoreHighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping (ScoperResult, getModuleId, scopeCheck)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource (ParserResult, fromSource)
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker (runTopModuleNameChecker)
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Extra (getScopedModuleTable)
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Scoped.Language (ScopedModuleTable)
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths
import Juvix.Prelude

data FormattedFileInfo = FormattedFileInfo
  { _formattedFileInfoPath :: Path Abs File,
    _formattedFileInfoContents :: Text,
    _formattedFileInfoContentsModified :: Bool
  }

type OriginalSource = Text

data ScopeEff :: Effect where
  ScopeFile :: Path Abs File -> ScopeEff m Scoper.ScoperResult
  ScopeStdin :: EntryPoint -> ScopeEff m Scoper.ScoperResult

makeLenses ''FormattedFileInfo
makeSem ''ScopeEff

data FormatResult
  = FormatResultOK
  | FormatResultNotFormatted
  | FormatResultFail
  deriving stock (Eq)

data SourceCode = SourceCode
  { _sourceCodeFormatted :: Text,
    _sourceCodeOriginal :: Text
  }

makeLenses ''SourceCode

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
  formattedContents :: Text <- runReader originalContents (formatPath p)
  let src =
        SourceCode
          { _sourceCodeFormatted = formattedContents,
            _sourceCodeOriginal = originalContents
          }
  formatResultSourceCode p src

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
formatProjectSourceCode ::
  forall r.
  (Members '[Output FormattedFileInfo] r) =>
  [(ImportNode, SourceCode)] ->
  Sem r FormatResult
formatProjectSourceCode =
  mconcatMapM (uncurry formatResultSourceCode)
    . map (first (^. importNodeAbsFile))

formatModuleInfo ::
  ( Members
      '[ PathResolver,
         Error JuvixError,
         Files,
         Reader Package
       ]
      r
  ) =>
  ImportNode ->
  PipelineResult Store.ModuleInfo ->
  Sem r SourceCode
formatModuleInfo node moduleInfo =
  withResolverRoot (node ^. importNodePackageRoot)
    . ignoreHighlightBuilder
    $ do
      pkg :: Package <- ask
      parseRes :: ParserResult <-
        runTopModuleNameChecker $
          fromSource Nothing (Just (node ^. importNodeAbsFile))
      let modules = moduleInfo ^. pipelineResultImports
          scopedModules :: ScopedModuleTable = getScopedModuleTable modules
          tmp :: TopModulePathKey = relPathtoTopModulePathKey (node ^. importNodeFile)
          moduleid :: ModuleId = run (runReader pkg (getModuleId tmp))
      scopeRes :: ScoperResult <-
        evalTopNameIdGen moduleid $
          scopeCheck pkg scopedModules parseRes
      originalSource :: Text <- readFile' (node ^. importNodeAbsFile)
      formattedTxt <-
        runReader originalSource $
          formatScoperResult False scopeRes
      let formatRes =
            SourceCode
              { _sourceCodeFormatted = formattedTxt,
                _sourceCodeOriginal = originalSource
              }
      return . forcing formatRes $ do
        forcesField sourceCodeFormatted
        forcesField sourceCodeOriginal

formatPath ::
  (Members '[Reader OriginalSource, ScopeEff] r) =>
  Path Abs File ->
  Sem r Text
formatPath p = do
  res <- scopeFile p
  formatScoperResult False res

formatStdin ::
  forall r.
  (Members '[Reader EntryPoint, ScopeEff, Files, Output FormattedFileInfo] r) =>
  Sem r FormatResult
formatStdin = do
  entry <- ask
  res <- scopeStdin entry
  let _sourceCodeOriginal = fromMaybe "" (entry ^. entryPointStdin)
  _sourceCodeFormatted :: Text <- runReader _sourceCodeOriginal (formatScoperResult False res)
  let src = SourceCode {..}
  formatResultSourceCode formatStdinPath src

formatResultSourceCode ::
  forall r.
  (Members '[Output FormattedFileInfo] r) =>
  Path Abs File ->
  SourceCode ->
  Sem r FormatResult
formatResultSourceCode filepath src = do
  if
      | src ^. sourceCodeOriginal /= src ^. sourceCodeFormatted -> mkResult FormatResultNotFormatted
      | otherwise -> mkResult FormatResultOK
  where
    mkResult :: FormatResult -> Sem r FormatResult
    mkResult res = do
      output
        ( FormattedFileInfo
            { _formattedFileInfoPath = filepath,
              _formattedFileInfoContents = src ^. sourceCodeFormatted,
              _formattedFileInfoContentsModified = res == FormatResultNotFormatted
            }
        )
      return res

formatScoperResult' ::
  Bool -> Text -> Scoper.ScoperResult -> Text
formatScoperResult' forceFormat original sres =
  run . runReader original $ formatScoperResult forceFormat sres

formatScoperResult ::
  (Members '[Reader OriginalSource] r) =>
  Bool ->
  Scoper.ScoperResult ->
  Sem r Text
formatScoperResult forceFormat res = do
  let comments = Scoper.getScoperResultComments res
      formattedTxt = toPlainTextTrim (ppOutDefault comments (res ^. Scoper.resultModule))
  runFailDefault formattedTxt $ do
    pragmas <- failMaybe (res ^. Scoper.mainModule . modulePragmas)
    PragmaFormat {..} <- failMaybe (pragmas ^. withLocParam . withSourceValue . pragmasFormat)
    failUnless (not _pragmaFormat && not forceFormat)
    ask @OriginalSource
