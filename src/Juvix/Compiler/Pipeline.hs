module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig,
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
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

type PipelineAppEffects = '[TaggedLock, Embed IO, Resource, Final IO]

type PipelineLocalEff = '[PathResolver, EvalFileEff, Error PackageLoaderError, Error DependencyError, GitClone, Error GitProcessError, Process, Log, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, HighlightBuilder, Internet]

-- type PipelineEff r = PipelineLocalEff ++ r
type PipelineEff r = PathResolver ': EvalFileEff ': Error PackageLoaderError ': Error DependencyError ': GitClone ': Error GitProcessError ': Process ': Log ': Reader EntryPoint ': Files ': NameIdGen ': Builtins ': Error JuvixError ': HighlightBuilder ': Internet ': r

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

upToSetup ::
  (Members '[Reader EntryPoint, Files, GitClone, PathResolver] r) =>
  DependenciesConfig ->
  Sem r ()
upToSetup = entrySetup

upToParsing ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, Error JuvixError, NameIdGen, GitClone, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = upToSetup defaultDependenciesConfig >> ask >>= Parser.fromSource

upToScoping ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToInternal ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, GitClone, PathResolver, Termination] r) =>
  Sem r Internal.InternalResult
upToInternal = upToScoping >>= Internal.fromConcrete

upToInternalTyped ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = Internal.typeCheckingNew upToInternal

upToInternalReachability ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  upToInternalTyped >>= Internal.filterUnreachable

upToCore ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCore = upToInternalReachability >>= Core.fromInternal

upToAsm ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToCore >>= \Core.CoreResult {..} -> coreToAsm _coreResultTable

upToMiniC ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToVampIR ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r VampIR.Result
upToVampIR =
  upToCore >>= \Core.CoreResult {..} -> coreToVampIR _coreResultTable

upToGeb ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToCore >>= \Core.CoreResult {..} -> coreToGeb spec _coreResultTable

upToCoreTypecheck ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck =
  upToCore >>= \r -> Core.toTypechecked (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

upToEval ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToEval =
  upToCore >>= \r -> Core.toEval (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

--------------------------------------------------------------------------------
-- Internal workflows
--------------------------------------------------------------------------------

coreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r Asm.InfoTable
coreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore

coreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

asmToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = Asm.toReg >=> regToMiniC . Reg.fromAsm

regToMiniC :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  e <- ask
  return $ C.fromReg (Backend.getLimits (e ^. entryPointTarget) (e ^. entryPointDebug)) tab

coreToGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Geb.ResultSpec -> Core.InfoTable -> Sem r Geb.Result
coreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore

coreToVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR = Core.toVampIR >=> VampIR.fromCore

asmToMiniC' :: (Members '[Error JuvixError, Reader Asm.Options] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: (Member (Reader Asm.Options) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab

coreToVampIR' :: (Members '[Error JuvixError, Reader Core.CoreOptions] r) => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR' = Core.toVampIR' >=> return . VampIR.fromCore' False
