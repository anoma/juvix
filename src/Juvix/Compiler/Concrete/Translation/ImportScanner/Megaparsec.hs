module Juvix.Compiler.Concrete.Translation.ImportScanner.Megaparsec
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
    scanBSImports,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight.Input
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
  (st, errm) <-
    ignoreHighlightBuilder
      . runParserResultBuilder mempty
      . ignoreTopModuleNameChecker
      $ runModuleParser fp (decodeUtf8 inputBS)
  m <- either throw return errm
  return
    ScanResult
      { _scanResultImports = hashSet . map fromImport $ st ^. parserStateImports,
        _scanResultModule = fromTopModulePath (m ^. modulePath)
      }
  where
    fromTopModulePath :: TopModulePath -> ScannedTopModuleName
    fromTopModulePath t =
      ScannedTopModuleName
        { _scannedTopModuleNameParts = unpack <$> topModulePathParts t,
          _scannedTopModuleLoc = getLoc t
        }

    fromImport :: Import 'Parsed -> ImportScan
    fromImport i = topModulePathToImportScan (i ^. importModulePath)
