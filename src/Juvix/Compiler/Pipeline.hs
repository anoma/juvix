module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root.Base,
    module Juvix.Compiler.Pipeline.Result,
  )
where

import Data.List.Singletons
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

type PipelineAppEffects = '[TaggedLock, Embed IO, Resource, Final IO]

type PipelineLocalEff = '[PathResolver, EvalFileEff, Error PackageLoaderError, Error DependencyError, GitClone, Error GitProcessError, Process, Log, Reader EntryPoint, Files, Error JuvixError, HighlightBuilder, Internet]

type PipelineEff' r = PipelineLocalEff ++ r

type PipelineEff r = Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': PipelineEff' r

--------------------------------------------------------------------------------
-- Workflows from source
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[Reader EntryPoint, Error JuvixError, Files, PathResolver] r) =>
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
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Scoper.ScoperResult
upToScoping = Scoper.fromParsed

upToInternal ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen, Termination] r) =>
  Sem r Internal.InternalResult
upToInternal = upToScoping >>= Internal.fromConcrete

upToInternalTyped ::
  (Members '[Reader Parser.ParserResult, Error JuvixError, Reader EntryPoint, Reader Store.ModuleTable, NameIdGen] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = Internal.typeCheckingNew upToInternal

upToCore ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCore = upToInternalTyped >>= Core.fromInternal

upToStoredCore ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToStoredCore =
  upToCore >>= \r -> Core.toStored (r ^. Core.coreResultModule) >>= \md -> return r {Core._coreResultModule = md}

upToAsm ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToAsm _coreResultModule

upToMiniC ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToVampIR ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r VampIR.Result
upToVampIR =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToVampIR _coreResultModule

upToGeb ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToStoredCore >>= \Core.CoreResult {..} -> storedCoreToGeb spec _coreResultModule

upToCoreTypecheck ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck =
  upToCore >>= \r -> Core.toTypechecked (r ^. Core.coreResultModule) >>= \md -> return r {Core._coreResultModule = md}

--------------------------------------------------------------------------------
-- Workflows from stored Core
--------------------------------------------------------------------------------

storedCoreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
storedCoreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore . Core.computeCombinedInfoTable

storedCoreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r C.MiniCResult
storedCoreToMiniC = storedCoreToAsm >=> asmToMiniC

storedCoreToGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Geb.ResultSpec -> Core.Module -> Sem r Geb.Result
storedCoreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore . Core.computeCombinedInfoTable

storedCoreToVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r VampIR.Result
storedCoreToVampIR = Core.toVampIR >=> VampIR.fromCore . Core.computeCombinedInfoTable

storedCoreToVampIR' :: (Members '[Error JuvixError, Reader Core.CoreOptions] r) => Core.Module -> Sem r VampIR.Result
storedCoreToVampIR' = Core.toVampIR' >=> return . VampIR.fromCore' False . Core.computeCombinedInfoTable

--------------------------------------------------------------------------------
-- Workflows from Core
--------------------------------------------------------------------------------

coreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
coreToAsm = Core.toStored >=> storedCoreToAsm

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

asmToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = Asm.toReg >=> regToMiniC . Reg.fromAsm

regToMiniC :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  e <- ask
  return $ C.fromReg (Backend.getLimits (e ^. entryPointTarget) (e ^. entryPointDebug)) tab

asmToMiniC' :: (Members '[Error JuvixError, Reader Asm.Options] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: (Member (Reader Asm.Options) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab
