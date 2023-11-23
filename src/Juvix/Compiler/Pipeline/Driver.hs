module Juvix.Compiler.Pipeline.Driver
  ( processFile,
    processFileUpTo,
    processFileUpTo',
    processFileToStoredCore,
    processFileToStoredCore',
    processModule,
    processImport,
  )
where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.CodeAnn
import Juvix.Data.Effect.Git
import Juvix.Prelude

processFile ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Parser.ParserResult, Store.ModuleTable)
processFile entry = do
  res <- runReader entry upToParsing
  let imports = res ^. Parser.resultParserState . Parser.parserStateImports
  modules <- forM imports (processImport entry)
  return (res, Store.mkModuleTable modules)

processImport ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Import 'Parsed ->
  Sem r Store.ModuleInfo
processImport entry i =
  withPath'
    i
    ( \path ->
        processModule (entry {_entryPointModulePath = Just path})
    )

processFileUpTo' ::
  forall r a.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  (forall r'. (Members PipelineEff' r') => Sem r' a) ->
  EntryPoint ->
  Sem r (a, Store.ModuleTable)
processFileUpTo' a entry = do
  (res, mtab) <- processFile entry
  fmap (,mtab)
    $ evalTopNameIdGen
      (res ^. Parser.resultModule . moduleId)
    $ runReader mtab
    $ runReader entry
    $ runReader res a

processFileUpTo ::
  forall r a.
  (Members '[Reader EntryPoint, Error JuvixError, Files, GitClone, PathResolver] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (a, Store.ModuleTable)
processFileUpTo a = do
  entry <- ask
  (res, mtab) <- processFile entry
  fmap (,mtab)
    $ evalTopNameIdGen
      (res ^. Parser.resultModule . moduleId)
    $ runReader mtab
    $ runReader res a

processFileToStoredCore' ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Core.CoreResult, Store.ModuleTable)
processFileToStoredCore' = processFileUpTo' upToStoredCore

processFileToStoredCore ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r Core.CoreResult
processFileToStoredCore entry = fst <$> processFileToStoredCore' entry

processModule ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r Store.ModuleInfo
processModule entry = mkModuleInfo <$> processFileToStoredCore entry
  where
    mkModuleInfo :: Core.CoreResult -> Store.ModuleInfo
    mkModuleInfo Core.CoreResult {..} =
      Store.ModuleInfo
        { _moduleInfoScopedModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . InternalArity.resultInternal . Internal.resultScoper . Scoper.resultScopedModule,
          _moduleInfoInternalModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternalModule,
          _moduleInfoCoreTable = fromCore _coreResultTable
        }

withPath' ::
  forall r a.
  (Members '[PathResolver, Error JuvixError] r) =>
  Import 'Parsed ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPath' i a = withPathFile (i ^. importModulePath) (either throwError a)
  where
    throwError :: PathResolverError -> Sem r a
    throwError e =
      mapError (JuvixError @GenericError) $
        throw
          GenericError
            { _genericErrorLoc = loc,
              _genericErrorMessage = mkAnsiText $ ppCodeAnn e,
              _genericErrorIntervals = [loc]
            }
      where
        loc = getLoc i
