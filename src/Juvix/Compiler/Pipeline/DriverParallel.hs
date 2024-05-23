module Juvix.Compiler.Pipeline.DriverParallel
  ( compileInParallel,
    ModuleInfoCache,
    evalModuleInfoCache,
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
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude
import Parallel.ParallelTemplate

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
         IOE,
         ModuleInfoCache,
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
  NumJobs ->
  Sem r ()
compileInParallel nj = do
  t <- ask
  idx <- mkNodesIndex t
  let args :: CompileArgs r NodeId Node CompileProof
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numJobs nj,
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
  NumJobs ->
  Sem (ModuleInfoCache ': r) a ->
  Sem r a
evalModuleInfoCache nj m = do
  Driver.evalModuleInfoCache $
    do
      compileInParallel nj
      m
