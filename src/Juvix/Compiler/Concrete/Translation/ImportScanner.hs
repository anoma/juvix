module Juvix.Compiler.Concrete.Translation.ImportScanner
  (
    scanImports,
    ScanResult(..),

                                                         ) where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete (ImportCycle (ImportCycle), ScoperError (ErrImportCycle))
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Prelude

data ModuleLocation = ModuleLocation {
  _moduleLocationPath :: Path File Abs,
  _modulePackageId :: Int
 }
  deriving stock (Eq, Data, Generic)


data ScanResult = ScanResult
  {
    scanModuleImports :: HashMap ModuleLocation (HashSet ModuleLocation)
  }

makeLenses ''ScanResult

data ScanError = ScanError

scanImports ::
  (Members '[Files] r) => EntryPoint -> Sem r ScanResult
scanImports = undefined

newtype ImportParents = ImportParents
  { _importParents :: [TopModulePath]
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ImportParents

newtype EntryIndex = EntryIndex
  { _entryIxEntry :: EntryPoint
  }

makeLenses ''EntryIndex

instance Eq EntryIndex where
  (==) = (==) `on` (^. entryIxEntry . entryPointModulePath)

instance Hashable EntryIndex where
  hashWithSalt s = hashWithSalt s . (^. entryIxEntry . entryPointModulePath)

processFileUpToParsing ::
  forall r.
  (Members '[HighlightBuilder, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r ()
processFileUpToParsing entry =
  runReader @ImportParents mempty
    . evalCacheEmpty processModule'
    $ processFileUpToParsing' entry

processFileUpToParsing' ::
  forall r.
  (Members '[HighlightBuilder, Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r ()
processFileUpToParsing' entry = do
  res <- runReader entry upToParsing
  let imports = res ^. Parser.resultParserState . Parser.parserStateImports
  processImports' entry (map (^. importModulePath) imports)

processImports' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r ()
processImports' entry imports = processImports'' entry imports

processImports'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r ()
processImports'' entry imports = do
  forM_ imports (processImport' entry)

processImport' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  TopModulePath ->
  Sem r ()
processImport' entry p = do
  checkCycle
  local (over importParents (p :)) $
    withPath' p getCachedImport
  where
    checkCycle :: Sem r ()
    checkCycle = do
      topp <- asks (^. importParents)
      case span (/= p) topp of
        (_, []) -> return ()
        (c, _) ->
          let cyc = NonEmpty.reverse (p :| c)
           in mapError (JuvixError @ScoperError) $
                throw (ErrImportCycle (ImportCycle cyc))

    getCachedImport :: Path Abs File -> Sem r ()
    getCachedImport path = processModule' (EntryIndex entry')
      where
        entry' =
          entry
            { _entryPointStdin = Nothing,
              _entryPointModulePath = Just path
            }

processFileToStoredCore' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r ()
processFileToStoredCore' entry = ignoreHighlightBuilder $ do
  processFileUpToParsing' entry

processModule' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryIndex ->
  Sem r ()
processModule' (EntryIndex entry) = do
  processModule'' entry
      -- saveToFile absPath (res ^. pipelineResult)

processModule'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r ()
processModule'' = ignoreHighlightBuilder . processFileUpToParsing

withPath' ::
  forall r a.
  (Members '[PathResolver, Error JuvixError] r) =>
  TopModulePath ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPath' path a = withPathFile path (either throwErr a)
  where
    throwErr :: PathResolverError -> Sem r a
    throwErr = mapError (JuvixError @PathResolverError) . throw
