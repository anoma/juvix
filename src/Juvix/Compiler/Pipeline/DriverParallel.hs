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

mkNodesIndex :: EntryPoint -> ImportTree -> NodesIndex NodeId Node
mkNodesIndex e tree =
  NodesIndex
    ( hashMap
        [ mkAssoc (fromNode ^. importNodeAbsFile)
          | fromNode <- HashMap.keys (tree ^. importTree)
        ]
    )
  where
    mkAssoc :: Path Abs File -> (Path Abs File, EntryIndex)
    mkAssoc p = (p, EntryIndex (set entryPointModulePath (Just p) e))

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
getNodeName (EntryIndex e) = pack (toFilePath (fromJust (e ^. entryPointModulePath)))

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
         Reader ImportTree
       ]
      r
  ) =>
  Sem r ()
compileInParallel = do
  entry <- ask
  t <- ask
  let idx = mkNodesIndex entry t
      args :: CompileArgs r NodeId Node CompileProof
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = 4,
            _compileArgsCompileNode = fmap force . processModule
          }
  void (compile args)

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
  Sem (ModuleInfoCache ': r) a ->
  Sem r a
evalModuleInfoCache m = do
  Driver.evalModuleInfoCache $
    do
      compileInParallel
      m
