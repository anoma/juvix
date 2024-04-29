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
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Prelude

scanBSImports ::
  (Members '[Error ParserError] r) =>
  Path Abs File ->
  ByteString ->
  Sem r (HashSet ImportScan)
scanBSImports fp inputBS = do
  let entry :: EntryPoint = impossible
  fmap (hashSet . map fromImport . (^. parserStateImports))
    . ignoreHighlightBuilder
    . execParserResultBuilder mempty
    . runReader entry
    $ runModuleParser fp (decodeUtf8 inputBS)
  where
    fromImport :: Import 'Parsed -> ImportScan
    fromImport i = topModulePathToImportScan (i ^. importModulePath)
