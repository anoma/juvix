module Juvix.Compiler.Pipeline.DriverParallel
  ( compileInParallel,
    compileInParallel_,
    formatInParallel,
    ModuleInfoCache,
    evalModuleInfoCache,
    module Parallel.ProgressLog,
    FormatResult,
    formatResultFormatted,
    formatResultOriginal,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent
import Juvix.Compiler.Concrete.Data.Highlight (ignoreHighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
import Juvix.Compiler.Concrete.Translation.FromSource (ParserResult, fromSource)
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
import Juvix.Formatter
import Juvix.Prelude
import Parallel.ParallelTemplate
import Parallel.ProgressLog

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultChanged :: Bool
  }

data FormatSourceResult = FormatSourceResult
  { _formatResultFormatted :: Text,
    _formatResultOriginal :: Text
  }

makeLenses ''CompileResult
makeLenses ''FormatSourceResult

type Node = EntryIndex

mkNodesIndex ::
  forall r.
  (Members '[Reader EntryPoint] r) =>
  ImportTree ->
  Sem r (NodesIndex ImportNode Node)
mkNodesIndex tree =
  NodesIndex
    . hashMap
    <$> sequence
      [ mkAssoc fromNode
        | fromNode <- HashMap.keys (tree ^. importTree)
      ]
  where
    mkAssoc :: ImportNode -> Sem r (ImportNode, EntryIndex)
    mkAssoc p = do
      i <- mkEntryIndex p
      return (p, i)

mkDependencies :: ImportTree -> Dependencies ImportNode
mkDependencies tree =
  Dependencies
    { _dependenciesTable = tree ^. importTree,
      _dependenciesTableReverse = tree ^. importTreeReverse
    }

getNodePath :: Node -> ImportNode
getNodePath = (^. entryIxImportNode)

getNodeName :: Node -> Text
getNodeName = toFilePath . (^. importNodeAbsFile) . getNodePath

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
         Reader NumThreads,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r ()
compileInParallel_ = void compileInParallel

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
         Reader NumThreads,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r (HashMap ImportNode (PipelineResult Store.ModuleInfo))
compileInParallel = do
  -- At the moment we compile everything, so the EntryIndex is ignored, but in
  -- principle we could only compile what is reachable from the given EntryIndex
  t <- ask
  idx <- mkNodesIndex t
  numWorkers <- ask >>= numThreads
  let args :: CompileArgs r ImportNode Node (PipelineResult Store.ModuleInfo)
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

formatInParallel ::
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
  Sem r (HashMap ImportNode FormatSourceResult)
formatInParallel nj = do
  t <- ask
  pkg <- asks (^. entryPointPackage)
  idx <- mkNodesIndex t
  root <- asks (^. entryPointRoot)
  numWorkers <- numThreads nj
  let args :: CompileArgs (Reader Package ': r) ImportNode Node FormatSourceResult
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsPreProcess = Nothing,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numWorkers,
            _compileArgsCompileNode = formatNode root
          }
  runReader pkg $
    compile args

-- TODO format only relevant modules
formatNode ::
  ( Members
      '[ ModuleInfoCache,
         PathResolver,
         Error JuvixError,
         Files,
         Reader Package
       ]
      r
  ) =>
  Path Abs Dir ->
  EntryIndex ->
  Sem r FormatSourceResult
formatNode _pkgRoot e = ignoreHighlightBuilder $ do
  moduleInfo :: PipelineResult Store.ModuleInfo <- compileNode e
  pkg :: Package <- ask
  parseRes :: ParserResult <-
    runTopModuleNameChecker $
      fromSource Nothing (Just (getNodePath e ^. importNodeAbsFile))
  let modules = moduleInfo ^. pipelineResultImports
      scopedModules :: ScopedModuleTable = getScopedModuleTable modules
      tmp :: TopModulePathKey = entryIndexTopModulePathKey e
  moduleid :: ModuleId <- runReader pkg (getModuleId tmp)
  scopeRes :: ScoperResult <-
    evalTopNameIdGen moduleid $
      scopeCheck pkg scopedModules parseRes
  originalSource :: Text <- readFile' (e ^. entryIxImportNode . importNodeAbsFile)
  formattedTxt <-
    runReader originalSource $
      formatScoperResult False scopeRes
  let formatRes =
        FormatSourceResult
          { _formatResultFormatted = formattedTxt,
            _formatResultOriginal = originalSource
          }
  return . forcing formatRes $
    when (True) $ do
      forcesField formatResultFormatted
      forcesField formatResultOriginal

compileNode ::
  (Members '[ModuleInfoCache, PathResolver] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
compileNode e =
  withResolverRoot (e ^. entryIxImportNode . importNodePackageRoot)
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
         Reader NumThreads,
         Files
       ]
      r
  ) =>
  Sem (ModuleInfoCache ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCache = Driver.evalModuleInfoCacheSetup (const (compileInParallel_))
