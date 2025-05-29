{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Compiler.Pipeline.Modular where

import Data.List.Singletons (type (++))
import Juvix.Compiler.Backend
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.Stripped.Module qualified as Stripped
import Juvix.Compiler.Core.Data.TransformationId qualified as Core
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Nockma.Data.Module qualified as Anoma
import Juvix.Compiler.Pipeline qualified as Pipeline
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Compiler.Store.Backend.Module qualified as Stored
import Juvix.Compiler.Store.Core.Extra qualified as Stored.Core
import Juvix.Compiler.Tree.Extra.Apply (addApplyBuiltins)
import Juvix.Compiler.Tree.Pipeline qualified as Tree
import Juvix.Compiler.Verification.Dumper
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Path qualified

type ModularEff r =
  '[ Files,
     TaggedLock,
     Reader EntryPoint,
     Error JuvixError,
     Dumper
   ]
    ++ r

type ModuleCache m = Cache ModuleId (PipelineResult m)

instance Serialize Core.Node where
  put = Serialize.put . Stored.Core.fromCoreNode
  get = Stored.Core.toCoreNode <$> Serialize.get

processModule ::
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t)] r) =>
  ModuleId ->
  Sem r (PipelineResult (Module' t))
processModule = cacheGet

processModuleCacheMiss ::
  forall t t' r.
  ( Monoid t',
    Serialize t',
    Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t')] r
  ) =>
  -- | If the target is 'Nothing', then the result is not saved on disk.
  Maybe Target ->
  ModuleTable' t ->
  ((ModuleId -> Sem r (Module' t')) -> t' -> Module' t -> Sem r (Module' t')) ->
  ModuleId ->
  Sem r (PipelineResult (Module' t'))
processModuleCacheMiss midTarget mt f mid = do
  entry <- ask
  let opts = Stored.fromEntryPoint entry
      md0 = lookupModuleTable mt mid
      sha256 = md0 ^. moduleSHA256
  res <- processImports (md0 ^. moduleImports)
  let changed = res ^. pipelineResultChanged
      imports = res ^. pipelineResult
  case midTarget of
    Nothing ->
      -- If the target is Nothing, we do not save the module on disk.
      -- This is useful for intermediate steps in the pipeline.
      recompile opts Nothing imports md0
    Just midTarget' ->
      let root = entry ^. entryPointRoot
          buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
          relPath =
            relFile
              ( sanitizeFilename (unpack $ prettyText mid)
                  <> getTargetExtension midTarget'
              )
          subdir = Stored.getOptionsSubdir midTarget' opts
          absPath = buildDir Path.</> subdir Path.</> relPath
       in if
              | changed ->
                  recompile opts (Just absPath) imports md0
              | otherwise -> do
                  mmd :: Maybe (Stored.Module' t') <- Serialize.loadFromFile absPath
                  case mmd of
                    Just md
                      | md ^. Stored.moduleSHA256 == sha256
                          && md ^. Stored.moduleOptions == opts
                          && md ^. Stored.moduleId == mid -> do
                          return
                            PipelineResult
                              { _pipelineResult = Stored.toBaseModule imports md,
                                _pipelineResultChanged = False
                              }
                      | otherwise -> recompile opts (Just absPath) imports md0
                    _ ->
                      recompile opts (Just absPath) imports md0
  where
    recompile :: Stored.Options -> Maybe (Path Abs File) -> [Module' t'] -> Module' t -> Sem r (PipelineResult (Module' t'))
    recompile opts absPath imports md0 = do
      let importsTab = mconcatMap computeCombinedInfoTable imports
      md :: Module' t' <- f fetchModule importsTab md0
      massert (md ^. moduleId == mid)
      massert (md ^. moduleSHA256 == md0 ^. moduleSHA256)
      whenJust absPath $ \absPath' ->
        Serialize.saveToFile absPath' (Stored.fromBaseModule opts md)
      return
        PipelineResult
          { _pipelineResult = md,
            _pipelineResultChanged = True
          }

    fetchModule :: ModuleId -> Sem r (Module' t')
    fetchModule =
      processModule >=> return . (^. pipelineResult)

processImports ::
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t)] r) =>
  [ModuleId] ->
  Sem r (PipelineResult [Module' t])
processImports mids = do
  res <- mapM processModule mids
  return
    PipelineResult
      { _pipelineResult = map (^. pipelineResult) res,
        _pipelineResultChanged = any (^. pipelineResultChanged) res
      }

processModuleTable' ::
  forall t t' r.
  (Serialize t', Monoid t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Maybe Target ->
  ((ModuleId -> Sem (ModuleCache (Module' t') ': r) (Module' t')) -> t' -> Module' t -> Sem (ModuleCache (Module' t') ': r) (Module' t')) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
processModuleTable' midTarget f mt = do
  tab <-
    evalCacheEmpty
      (processModuleCacheMiss midTarget mt f)
      $ mapM (fmap (^. pipelineResult) . processModule . (^. moduleId)) (mt ^. moduleTable)
  return $ ModuleTable tab

processModuleTable ::
  forall t t' r.
  (Serialize t', Monoid t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Maybe Target ->
  (Module' t -> Sem r (Module' t')) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
processModuleTable midTarget f mt =
  processModuleTable' midTarget (const goModule) mt
  where
    goModule :: t' -> Module' t -> Sem (ModuleCache (Module' t') ': r) (Module' t')
    goModule importsTab md = do
      md' <- inject (f md)
      massert (md' ^. moduleId == md ^. moduleId)
      massert (md' ^. moduleSHA256 == md ^. moduleSHA256)
      return md' {_moduleImportsTable = importsTab}

modularCoreToPreStripped ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Core.ModuleTable ->
  Sem r Core.ModuleTable
modularCoreToPreStripped mt =
  processModuleTable Nothing (Pipeline.storedCoreToPreStripped Core.IdentityTrans) mt

modularPreStrippedToStripped ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Core.ModuleTable ->
  Sem r Stripped.ModuleTable
modularPreStrippedToStripped mt =
  processModuleTable (Just TargetStripped) Pipeline.preStrippedToStripped mt

modularCoreToStripped ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Core.ModuleTable ->
  Sem r Stripped.ModuleTable
modularCoreToStripped =
  modularCoreToPreStripped >=> modularPreStrippedToStripped

modularStrippedToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Stripped.ModuleTable ->
  Sem r Tree.ModuleTable
modularStrippedToTree mt =
  processModuleTable (Just TargetTree) Pipeline.strippedCoreToTree mt

modularTreeToAnoma ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Tree.ModuleTable ->
  Sem r Anoma.ModuleTable
modularTreeToAnoma mt =
  processModuleTable' (Just TargetAnoma) Pipeline.treeToAnoma' (addApplyBuiltins mt)

modularCoreToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Core.ModuleTable ->
  Sem r Tree.ModuleTable
modularCoreToTree =
  modularCoreToStripped >=> modularStrippedToTree

modularCoreToAnoma ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, Dumper] r) =>
  Core.ModuleTable ->
  Sem r Anoma.ModuleTable
modularCoreToAnoma =
  modularCoreToTree >=> modularTreeToAnoma
