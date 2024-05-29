module Juvix.Compiler.Pipeline.DriverParallel
  ( compileInParallel,
    ModuleInfoCache,
    evalModuleInfoCache,
    module Parallel.ProgressLog,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner (ImportScanStrategy)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver (processModule)
import Juvix.Compiler.Pipeline.Driver qualified as Driver
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude
import Parallel.ParallelTemplate
import Parallel.ProgressLog

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultChanged :: Bool
  }

makeLenses ''CompileResult

type NodeId = Path Abs File

type Node = EntryIndex

type CompileProof = PipelineResult Store.ModuleInfo

mkNodesIndex :: forall r. (Members '[Reader EntryPoint] r) => ImportTree -> Sem r (NodesIndex NodeId Node)
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
      i <- mkEntryIndex (p ^. importNodePackageRoot) abspath
      return (abspath, i)

mkDependencies :: ImportTree -> Dependencies NodeId
mkDependencies tree =
  Dependencies
    { _dependenciesTable = helper (tree ^. importTree),
      _dependenciesTableReverse = helper (tree ^. importTreeReverse)
    }
  where
    helper :: HashMap ImportNode (HashSet ImportNode) -> HashMap NodeId (HashSet NodeId)
    helper m = hashMap [(toPath k, hashSet (toPath <$> toList v)) | (k, v) <- HashMap.toList m]

    toPath :: ImportNode -> Path Abs File
    toPath = (^. importNodeAbsFile)

getNodeName :: Node -> Text
getNodeName = toFilePath . fromJust . (^. entryIxEntry . entryPointModulePath)

-- | Fills the cache in parallel
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
  EntryIndex ->
  Sem r ()
compileInParallel nj _entry = do
  -- At the moment we compile everything, so the EntryIndex is ignored, but in
  -- principle we could only compile what is reachable from the given EntryIndex
  t <- ask
  idx <- mkNodesIndex t
  numWorkers <- numThreads nj
  let args :: CompileArgs r NodeId Node CompileProof
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsPreProcess = Just preLoadFromFile,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numWorkers,
            _compileArgsCompileNode = compileNode
          }
  void (compile args)

compileNode :: (Members '[ModuleInfoCache, PathResolver] r) => EntryIndex -> Sem r CompileProof
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
evalModuleInfoCache nj = Driver.evalModuleInfoCacheSetup (compileInParallel nj)
