module Juvix.Compiler.Pipeline.Driver
  ( processFile,
    processFileToStoredCore,
    processModule,
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
  modules <- forM imports goImport
  return (res, Store.mkModuleTable modules)
  where
    goImport :: Import 'Parsed -> Sem r Store.ModuleInfo
    goImport i =
      withPath'
        i
        ( \path ->
            processModule (entry {_entryPointModulePath = Just path})
        )

processFileToStoredCore ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r Core.CoreResult
processFileToStoredCore entry = do
  (res, mtab) <- processFile entry
  runReader res $
    runReader entry $
      runReader mtab $
        evalTopNameIdGen
          (res ^. Parser.resultModule . moduleId)
          upToStoredCore

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
          _moduleInfoCoreTable = undefined
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
