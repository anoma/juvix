module Juvix.Compiler.Pipeline.DriverParallel
  ( module Juvix.Compiler.Pipeline.DriverParallel.Base,
    processFileUpTo,
    compileInParallel,
    processFileToStoredCore,
    ModuleInfoCache,
    evalModuleInfoCache,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty (ppTrace)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping (getModuleId)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error (ScoperError)
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner (ImportScanStrategy)
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.DriverParallel.Base
import Juvix.Compiler.Pipeline.ImportParents (ImportParents)
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Serialize
import Juvix.Parser.Error (ParserError)
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Parallel.ParallelTemplate
import Path.Posix qualified as Path

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

compileNode ::
  ( Members
      '[ Reader ImportTree,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         Files
       ]
      r
  ) =>
  HashMap NodeId CompileProof ->
  Node ->
  Sem r CompileProof
compileNode k node = do
  tree <- ask @ImportTree
  traceM
    ( "compileNode: "
        <> getNodeName node
        <> "\ndeps available: "
        <> show (HashMap.keys k)
    )
  -- traceM (ppTrace tree)
  runReader k (processModule node)

getNodeName :: Node -> Text
getNodeName (EntryIndex e) = show (fromJust (e ^. entryPointModulePath))

compileInParallel ::
  forall r a.
  ( Members
      '[ Concurrent,
         IOE,
         TaggedLock,
         Files,
         TopModuleNameChecker,
         Error JuvixError,
         Reader EntryPoint,
         Reader ImportTree
       ]
      r
  ) =>
  Sem (ImportsAccess ': r) a ->
  Sem r a
compileInParallel m = do
  e <- ask
  t <- ask
  let idx = mkNodesIndex e t
      args :: CompileArgs r NodeId Node CompileProof
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = 1,
            _compileArgsCompileNode = compileNode
          }
  res <- compile args
  runReader res m

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

-- TODO there must be a more elegant way to handle this (i.e. what are
-- ModuleInfoCache and ImportParents doing here?)
evalModuleInfoCache ::
  forall r a.
  ( Members
      '[ Reader EntryPoint,
         IOE,
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
  Sem (ModuleInfoCache ': Reader ImportParents ': r) a ->
  Sem r a
evalModuleInfoCache m = do
  e <- ask
  tree <-
    mapError (JuvixError @ParserError)
      . mapError (JuvixError @ScoperError)
      $ mkImportTree (e ^. entryPointModulePath)
  runReader @ImportParents mempty
    . runReader tree
    . compileInParallel
    . evalCacheEmpty processModule
    $ inject m

processFileUpTo ::
  forall r a.
  ( Members
      '[ Reader ImportTree,
         ImportsAccess,
         TaggedLock,
         TopModuleNameChecker,
         HighlightBuilder,
         Reader EntryPoint,
         Error JuvixError,
         Files,
         ModuleInfoCache
       ]
      r
  ) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFileUpToParsing entry
  mid <- getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath)
  a' <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ a
  return (set pipelineResult a' res)

processFileUpToParsing ::
  forall r.
  ( Members
      '[ Reader ImportTree,
         ImportsAccess,
         HighlightBuilder,
         TopModuleNameChecker,
         Error JuvixError,
         Files
       ]
      r
  ) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry = do
  res <- runReader entry upToParsing
  mtab <- (^. compileResultModuleTable) <$> processImports entry
  return
    PipelineResult
      { _pipelineResult = res,
        _pipelineResultImports = mtab,
        _pipelineResultChanged = True
      }

processImports ::
  forall r.
  (Members '[Reader ImportTree, ImportsAccess] r) =>
  EntryPoint ->
  Sem r CompileResult
processImports e = do
  tree <- ask @ImportTree
  tbl <- ask @(HashMap (Path Abs File) (PipelineResult Store.ModuleInfo))
  node :: ImportNode <- entryPointNode e
  let deps :: HashSet ImportNode = fromMaybe err (tree ^. importTree . at node)
      err :: HashSet ImportNode = error ("unexpected: could not find deps for node " <> show node)
      depsPaths :: [Path Abs File] = (^. importNodeAbsFile) <$> toList deps
      getDep :: Path Abs File -> PipelineResult Store.ModuleInfo
      getDep p = fromJust (tbl ^. at p)
      depsInfos = map getDep depsPaths
      mtab =
        Store.mkModuleTable (map (^. pipelineResult) depsInfos)
          <> mconcatMap (^. pipelineResultImports) depsInfos
      changed = any (^. pipelineResultChanged) depsInfos
  return
    CompileResult
      { _compileResultChanged = changed,
        _compileResultModuleTable = mtab
      }

entryPointNode :: (Members '[Reader ImportTree] r) => EntryPoint -> Sem r ImportNode
entryPointNode e = do
  t <- ask
  return (fromMaybe (err t) (t ^. importTreeFiles . at srcFile))
  where
    err :: ImportTree -> a
    err t =
      error $
        "Could not find srcFile:\n"
          <> show srcFile
          <> "in node collection "
          <> show (t ^. importTreeFiles)
          <> "\n"
    srcFile = fromJust (e ^. entryPointModulePath)

processFileToStoredCore ::
  forall r.
  ( Members
      '[ Reader ImportTree,
         ImportsAccess,
         TopModuleNameChecker,
         Error JuvixError,
         Files
       ]
      r
  ) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry = ignoreHighlightBuilder . runReader entry $ do
  res <- processFileUpToParsing entry
  mid <- getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath)
  r <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)

traceImports :: (Members '[Reader ImportTree, ImportsAccess] r) => Path Abs File -> Sem r ()
traceImports file = do
  tree <- ask
  tbl <- ask @(HashMap (Path Abs File) (PipelineResult Store.ModuleInfo))
  let node :: ImportNode = fromJust (tree ^. importTreeFiles . at file)
      deps :: HashSet ImportNode = fromJust (tree ^. importTree . at node)
  traceM
    ( pack (toFilePath file)
        <> "\nDepends on:\n"
        <> show (prettyText <$> toList deps)
    )
  traceM (pack (toFilePath file) <> "\nhas Access to:\n" <> show (toFilePath <$> HashMap.keys tbl))

processModule ::
  forall r.
  ( Members
      '[ Reader ImportTree,
         ImportsAccess,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         Files
       ]
      r
  ) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule (EntryIndex entry) = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      sourcePath = fromJust (entry ^. entryPointModulePath)
      relPath =
        fromJust
          . replaceExtension ".jvo"
          . fromJust
          $ stripProperPrefix $(mkAbsDir "/") sourcePath
      absPath = buildDir Path.</> relPath
  traceImports sourcePath
  sha256 <- SHA256.digestFile sourcePath
  m :: Maybe Store.ModuleInfo <- loadFromFile absPath
  case m of
    Just info
      | info ^. Store.moduleInfoSHA256 == sha256
          && info ^. Store.moduleInfoOptions == opts
          && info ^. Store.moduleInfoFieldSize == entry ^. entryPointFieldSize -> do
          CompileResult {..} <- processImports entry
          if
              | _compileResultChanged ->
                  recompile sha256 absPath
              | otherwise ->
                  return
                    PipelineResult
                      { _pipelineResult = info,
                        _pipelineResultImports = _compileResultModuleTable,
                        _pipelineResultChanged = False
                      }
    _ ->
      recompile sha256 absPath
  where
    root = entry ^. entryPointRoot
    opts = StoredModule.fromEntryPoint entry

    recompile :: Text -> Path Abs File -> Sem r (PipelineResult Store.ModuleInfo)
    recompile sha256 absPath = do
      res <- processModuleToStoredCore sha256 entry
      saveToFile absPath (res ^. pipelineResult)
      return res

processModuleToStoredCore ::
  forall r.
  ( Members
      '[ Reader ImportTree,
         ImportsAccess,
         TopModuleNameChecker,
         Error JuvixError,
         Files
       ]
      r
  ) =>
  Text ->
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleToStoredCore sha256 entry = over pipelineResult mkModuleInfo <$> processFileToStoredCore entry
  where
    mkModuleInfo :: Core.CoreResult -> Store.ModuleInfo
    mkModuleInfo Core.CoreResult {..} =
      Store.ModuleInfo
        { _moduleInfoScopedModule = scoperResult ^. Scoper.resultScopedModule,
          _moduleInfoInternalModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternalModule,
          _moduleInfoCoreTable = fromCore (_coreResultModule ^. Core.moduleInfoTable),
          _moduleInfoImports = map (^. importModulePath) $ scoperResult ^. Scoper.resultParserResult . Parser.resultParserState . parserStateImports,
          _moduleInfoOptions = StoredOptions.fromEntryPoint entry,
          _moduleInfoSHA256 = sha256,
          _moduleInfoFieldSize = entry ^. entryPointFieldSize
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper
