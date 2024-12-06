module Juvix.Compiler.Concrete.Translation.ImportScanner.Megaparsec
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
    scanBSImports,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight.Builder
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Prelude

scanBSImports ::
  (Members '[Error ParserError] r) =>
  Path Abs File ->
  ByteString ->
  Sem r ScanResult
scanBSImports fp inputBS = do
  st <-
    evalHighlightBuilder
      . execParserResultBuilder mempty
      . ignoreTopModuleNameChecker
      $ runModuleParser fp (decodeUtf8 inputBS)
  return
    ScanResult
      { _scanResultImports = hashSet . map fromImport $ st ^. parserStateImports
      }
  where
    fromImport :: Import 'Parsed -> ImportScan
    fromImport i = topModulePathToImportScan (i ^. importModulePath)
