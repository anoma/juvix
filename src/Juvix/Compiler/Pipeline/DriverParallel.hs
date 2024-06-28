module Juvix.Compiler.Pipeline.DriverParallel
  ( compileInParallel,
    compileInParallel_,
    scopeInParallel,
    ModuleInfoCache,
    evalModuleInfoCache,
    module Parallel.ProgressLog,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent
import Juvix.Compiler.Concrete.Data.Highlight (ignoreHighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
import Juvix.Compiler.Concrete.Translation.FromSource
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner (ImportScanStrategy)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver (processModule)
import Juvix.Compiler.Pipeline.Driver qualified as Driver
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude
import Parallel.ParallelTemplate
import Parallel.ProgressLog

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultChanged :: Bool
  }

data ScopingResult = ScopingResult
  { _scopingResultModuleInfo :: PipelineResult Store.ModuleInfo,
    _scopingResultScoperResult :: ScoperResult
  }

makeLenses ''CompileResult

type Node = EntryIndex

mkNodesIndex ::
  forall r.
  (Members '[Reader EntryPoint] r) =>
  ImportTree ->
  Sem r (NodesIndex (Path Abs File) Node)
mkNodesIndex tree =
  NodesIndex
    . hashMap
    <$> sequence
      [ mkAssoc fromNode
        | fromNode <- HashMap.keys (tree ^. importTree)
      ]
  where
    mkAssoc :: ImportNode -> Sem r (Path Abs File, EntryIndex)
    mkAssoc p = do
      let abspath = p ^. importNodeAbsFile
          moduleKey = relPathtoTopModulePathKey (p ^. importNodeFile)
      i <- mkEntryIndex moduleKey (p ^. importNodePackageRoot) abspath
      return (abspath, i)

mkDependencies :: ImportTree -> Dependencies (Path Abs File)
mkDependencies tree =
  Dependencies
    { _dependenciesTable = helper (tree ^. importTree),
      _dependenciesTableReverse = helper (tree ^. importTreeReverse)
    }
  where
    helper :: HashMap ImportNode (HashSet ImportNode) -> HashMap (Path Abs File) (HashSet (Path Abs File))
    helper m = hashMap [(toPath k, hashSet (toPath <$> toList v)) | (k, v) <- HashMap.toList m]

    toPath :: ImportNode -> Path Abs File
    toPath = (^. importNodeAbsFile)

getNodePath :: Node -> Path Abs File
getNodePath = fromJust . (^. entryIxEntry . entryPointModulePath)

getNodeName :: Node -> Text
getNodeName = toFilePath . getNodePath

compileInParallel_ ::
  forall r.
  ( Members
      '[ Concurrent,
         ProgressLog,
         IOE,
         ModuleInfoCache,
         JvoCache,
         TaggedLock,
         Files,
         TopModuleNameChecker,
         Error JuvixError,
         Reader EntryPoint,
         PathResolver,
         Reader ImportTree
       ]
      r
  ) =>
  NumThreads ->
  Sem r ()
compileInParallel_ = void . compileInParallel

-- | Compiles the whole project in parallel (i.e. all modules in the ImportTree).
compileInParallel ::
  forall r.
  ( Members
      '[ Concurrent,
         ProgressLog,
         IOE,
         ModuleInfoCache,
         JvoCache,
         TaggedLock,
         Files,
         TopModuleNameChecker,
         Error JuvixError,
         Reader EntryPoint,
         PathResolver,
         Reader ImportTree
       ]
      r
  ) =>
  NumThreads ->
  Sem r (HashMap (Path Abs File) (PipelineResult Store.ModuleInfo))
compileInParallel nj = do
  -- At the moment we compile everything, so the EntryIndex is ignored, but in
  -- principle we could only compile what is reachable from the given EntryIndex
  t <- ask
  idx <- mkNodesIndex t
  numWorkers <- numThreads nj
  let args :: CompileArgs r (Path Abs File) Node (PipelineResult Store.ModuleInfo)
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsPreProcess = Just preLoadFromFile,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numWorkers,
            _compileArgsCompileNode = compileNode
          }
  compile args

scopeInParallel ::
  forall r.
  ( Members
      '[ Concurrent,
         ProgressLog,
         IOE,
         JvoCache,
         TaggedLock,
         ModuleInfoCache,
         Files,
         TopModuleNameChecker,
         Error JuvixError,
         Reader EntryPoint,
         PathResolver,
         Reader ImportTree
       ]
      r
  ) =>
  NumThreads ->
  Sem r (HashMap (Path Abs File) ScopingResult)
scopeInParallel nj = do
  t <- ask
  pkg <- asks (^. entryPointPackage)
  idx <- mkNodesIndex t
  numWorkers <- numThreads nj
  let args :: CompileArgs (Reader Package ': r) (Path Abs File) Node ScopingResult
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsPreProcess = Nothing,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numWorkers,
            _compileArgsCompileNode = scopeNode
          }
  runReader pkg $
    compile args

scopeNode ::
  ( Members
      '[ ModuleInfoCache,
         PathResolver,
         Error JuvixError,
         Files,
         Reader Package
       ]
      r
  ) =>
  EntryIndex ->
  Sem r ScopingResult
scopeNode e = ignoreHighlightBuilder $ do
  moduleInfo :: PipelineResult Store.ModuleInfo <- compileNode e
  pkg :: Package <- ask
  parseRes :: ParserResult <-
    runTopModuleNameChecker $
      fromSource Nothing (Just (getNodePath e))
  let modules = moduleInfo ^. pipelineResultImports
      scopedModules :: ScopedModuleTable = getScopedModuleTable modules
      tmp :: TopModulePathKey = e ^. entryIxTopModulePathKey
  moduleid :: ModuleId <- runReader pkg (getModuleId tmp)
  scopeRes <-
    evalTopNameIdGen moduleid $
      scopeCheck pkg scopedModules parseRes
  -- FIXME need to apply `force`
  return
    ScopingResult
      { _scopingResultModuleInfo = moduleInfo,
        _scopingResultScoperResult = scopeRes
      }

compileNode :: (Members '[ModuleInfoCache, PathResolver] r) => EntryIndex -> Sem r (PipelineResult Store.ModuleInfo)
compileNode e =
  withResolverRoot (e ^. entryIxResolverRoot)
    . fmap force
    $ processModule e

instance Semigroup CompileResult where
  sconcat l =
    CompileResult
      { _compileResultChanged = any (^. compileResultChanged) l,
        _compileResultModuleTable = sconcatMap (^. compileResultModuleTable) l
      }

instance Monoid CompileResult where
  mempty =
    CompileResult
      { _compileResultChanged = False,
        _compileResultModuleTable = mempty
      }

evalModuleInfoCache ::
  forall r a.
  ( Members
      '[ Reader EntryPoint,
         IOE,
         ProgressLog,
         Reader ImportTree,
         Concurrent,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         PathResolver,
         Reader ImportScanStrategy,
         Files
       ]
      r
  ) =>
  NumThreads ->
  Sem (ModuleInfoCache ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCache nj = Driver.evalModuleInfoCacheSetup (const (compileInParallel_ nj))
