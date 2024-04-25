module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root.Base,
    module Juvix.Compiler.Pipeline.Result,
  )
where

import Data.List.Singletons (type (++))
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromTree qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Cairo qualified as Cairo
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Casm.Data.Builtins qualified as Casm
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Translation.FromReg qualified as Casm
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Nockma.Translation.FromTree qualified as NockmaTree
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.ImportParents
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Compiler.Reg.Pipeline qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Tree qualified as Tree
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.Field

type PipelineAppEffects = '[TaggedLock, EmbedIO]

type PipelineLocalEff = '[ModuleInfoCache, Reader ImportParents, PathResolver, EvalFileEff, Error PackageLoaderError, Error DependencyError, GitClone, Error GitProcessError, Process, Log, Reader EntryPoint, Files, Error JuvixError, HighlightBuilder, Internet]

type PipelineEff' r = PipelineLocalEff ++ r

type PipelineEff r = Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': PipelineEff' r

--------------------------------------------------------------------------------
-- Workflows from source
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[HighlightBuilder, Reader EntryPoint, Error JuvixError, Files, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = ask >>= Parser.fromSource

--------------------------------------------------------------------------------
-- Workflows from parsed source
--------------------------------------------------------------------------------

upToParsedSource ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Parser.ParserResult
upToParsedSource = ask

upToScoping ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Scoper.ScoperResult
upToScoping = Scoper.fromParsed

upToInternal ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen, Termination] r) =>
  Sem r Internal.InternalResult
upToInternal = upToScoping >>= Internal.fromConcrete

upToInternalTyped ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Error JuvixError, Reader EntryPoint, Reader Store.ModuleTable, NameIdGen] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = Internal.typeCheckingNew upToInternal

upToCore ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Core.CoreResult
upToCore = upToInternalTyped >>= Core.fromInternal

upToStoredCore ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Core.CoreResult
upToStoredCore = do
  r <- upToCore
  md <- Core.toStored (r ^. Core.coreResultModule)
  return r {Core._coreResultModule = md}

upToReg ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Reg.InfoTable
upToReg =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToReg _coreResultModule

upToTree ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Tree.InfoTable
upToTree =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToTree Core.Identity _coreResultModule

upToAsm ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToAsm _coreResultModule

upToCasm ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Casm.Result
upToCasm =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToCasm _coreResultModule

upToCairo ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Cairo.Result
upToCairo =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToCairo _coreResultModule

upToMiniC ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToVampIR ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r VampIR.Result
upToVampIR =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToVampIR _coreResultModule

upToGeb ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToGeb spec _coreResultModule

upToAnoma ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r NockmaTree.AnomaResult
upToAnoma = upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToAnoma _coreResultModule

upToCoreTypecheck ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck = do
  r <- upToCore
  md <- Core.toTypechecked (r ^. Core.coreResultModule)
  return r {Core._coreResultModule = md}

--------------------------------------------------------------------------------
-- Workflows from stored Core
--------------------------------------------------------------------------------

storedCoreToTree ::
  (Members '[Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.Module ->
  Sem r Tree.InfoTable
storedCoreToTree checkId md = do
  fsize <- asks (^. entryPointFieldSize)
  Tree.fromCore . Stripped.fromCore fsize . Core.computeCombinedInfoTable <$> Core.toStripped checkId md

storedCoreToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r NockmaTree.AnomaResult
storedCoreToAnoma = storedCoreToTree Core.CheckAnoma >=> treeToAnoma

storedCoreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
storedCoreToAsm = storedCoreToTree Core.CheckExec >=> treeToAsm

storedCoreToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Reg.InfoTable
storedCoreToReg = storedCoreToAsm >=> asmToReg

storedCoreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r C.MiniCResult
storedCoreToMiniC = storedCoreToAsm >=> asmToMiniC

storedCoreToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Casm.Result
storedCoreToCasm = local (set entryPointFieldSize cairoFieldSize) . storedCoreToTree Core.CheckCairo >=> treeToCasm

storedCoreToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Cairo.Result
storedCoreToCairo = storedCoreToCasm >=> casmToCairo

storedCoreToGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Geb.ResultSpec -> Core.Module -> Sem r Geb.Result
storedCoreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore . Core.computeCombinedInfoTable

storedCoreToVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r VampIR.Result
storedCoreToVampIR = Core.toVampIR >=> VampIR.fromCore . Core.computeCombinedInfoTable

storedCoreToVampIR' :: (Members '[Error JuvixError, Reader Core.CoreOptions] r) => Core.Module -> Sem r VampIR.Result
storedCoreToVampIR' = Core.toVampIR' >=> return . VampIR.fromCore' False . Core.computeCombinedInfoTable

--------------------------------------------------------------------------------
-- Workflows from Core
--------------------------------------------------------------------------------

coreToTree :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.TransformationId -> Core.Module -> Sem r Tree.InfoTable
coreToTree checkId = Core.toStored >=> storedCoreToTree checkId

coreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
coreToAsm = Core.toStored >=> storedCoreToAsm

coreToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Reg.InfoTable
coreToReg = Core.toStored >=> storedCoreToReg

coreToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Casm.Result
coreToCasm = Core.toStored >=> storedCoreToCasm

coreToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Cairo.Result
coreToCairo = Core.toStored >=> storedCoreToCairo

coreToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r NockmaTree.AnomaResult
coreToAnoma = coreToTree Core.CheckAnoma >=> treeToAnoma

coreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

coreToGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Geb.ResultSpec -> Core.Module -> Sem r Geb.Result
coreToGeb spec = Core.toStored >=> storedCoreToGeb spec

coreToVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r VampIR.Result
coreToVampIR = Core.toStored >=> storedCoreToVampIR

coreToVampIR' :: (Members '[Error JuvixError, Reader Core.CoreOptions] r) => Core.Module -> Sem r VampIR.Result
coreToVampIR' = Core.toStored' >=> storedCoreToVampIR'

--------------------------------------------------------------------------------
-- Other workflows
--------------------------------------------------------------------------------

treeToAsm :: (Member (Error JuvixError) r) => Tree.InfoTable -> Sem r Asm.InfoTable
treeToAsm = Tree.toAsm >=> return . Asm.fromTree

treeToCairoAsm :: (Member (Error JuvixError) r) => Tree.InfoTable -> Sem r Asm.InfoTable
treeToCairoAsm = Tree.toCairoAsm >=> return . Asm.fromTree

treeToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Reg.InfoTable
treeToReg = treeToAsm >=> asmToReg

treeToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r (NockmaTree.AnomaResult)
treeToAnoma = Tree.toNockma >=> mapReader NockmaTree.fromEntryPoint . NockmaTree.fromTreeTable

treeToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r C.MiniCResult
treeToMiniC = treeToAsm >=> asmToMiniC

treeToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Casm.Result
treeToCasm = treeToCairoAsm >=> asmToCasm

treeToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Cairo.Result
treeToCairo = treeToCasm >=> casmToCairo

asmToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Reg.InfoTable
asmToReg = Asm.toReg >=> return . Reg.fromAsm

asmToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Casm.Result
asmToCasm = asmToReg >=> regToCasm

asmToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Cairo.Result
asmToCairo = asmToReg >=> regToCairo

asmToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = asmToReg >=> regToMiniC

regToMiniC :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  tab' <- Reg.toC tab
  e <- ask
  return $ C.fromReg (Backend.getLimits (getEntryPointTarget e) (e ^. entryPointDebug)) tab'

regToCasm :: Reg.InfoTable -> Sem r Casm.Result
regToCasm = Reg.toCasm >=> return . Casm.fromReg

casmToCairo :: Casm.Result -> Sem r Cairo.Result
casmToCairo Casm.Result {..} =
  return
    . Cairo.serialize (map Casm.builtinName _resultBuiltins)
    $ Cairo.fromCasm _resultCode

regToCairo :: Reg.InfoTable -> Sem r Cairo.Result
regToCairo = regToCasm >=> casmToCairo

treeToAnoma' :: (Members '[Error JuvixError, Reader NockmaTree.CompilerOptions] r) => Tree.InfoTable -> Sem r NockmaTree.AnomaResult
treeToAnoma' = Tree.toNockma >=> NockmaTree.fromTreeTable

asmToMiniC' :: (Members '[Error JuvixError, Reader Asm.Options] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: (Member (Reader Asm.Options) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab
