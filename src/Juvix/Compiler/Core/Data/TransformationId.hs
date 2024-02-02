module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Core.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | LetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToPrimInt
  | IntToPrimInt
  | ConvertBuiltinTypes
  | Identity
  | UnrollRecursion
  | ComputeTypeInfo
  | MatchToCase
  | NaiveMatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CombineInfoTables
  | CheckGeb
  | CheckExec
  | CheckVampIR
  | Normalize
  | LetFolding
  | LambdaFolding
  | LetHoisting
  | Inlining
  | MandatoryInlining
  | FoldTypeSynonyms
  | CaseCallLifting
  | SimplifyIfs
  | SimplifyComparisons
  | SpecializeArgs
  | CaseFolding
  | CasePermutation
  | FilterUnreachable
  | OptPhaseEval
  | OptPhaseExec
  | OptPhaseGeb
  | OptPhaseVampIR
  | OptPhaseMain
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineStored
  | PipelineNormalize
  | PipelineGeb
  | PipelineVampIR
  | PipelineStripped
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations = [MatchToCase]

toStoredTransformations :: [TransformationId]
toStoredTransformations = [EtaExpandApps, MatchToCase, NatToPrimInt, IntToPrimInt, ConvertBuiltinTypes, OptPhaseEval, DisambiguateNames]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations = [CombineInfoTables, LetRecLifting, LetFolding, UnrollRecursion]

toVampIRTransformations :: [TransformationId]
toVampIRTransformations = [CombineInfoTables, FilterUnreachable, CheckVampIR, LetRecLifting, OptPhaseVampIR, UnrollRecursion, Normalize, LetHoisting]

toStrippedTransformations :: [TransformationId]
toStrippedTransformations =
  [CombineInfoTables, FilterUnreachable, CheckExec, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps, RemoveTypeArgs]

toGebTransformations :: [TransformationId]
toGebTransformations = [CombineInfoTables, FilterUnreachable, CheckGeb, LetRecLifting, OptPhaseGeb, UnrollRecursion, FoldTypeSynonyms, ComputeTypeInfo]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    LambdaLetRecLifting -> strLifting
    LetRecLifting -> strLetRecLifting
    TopEtaExpand -> strTopEtaExpand
    MatchToCase -> strMatchToCase
    NaiveMatchToCase -> strNaiveMatchToCase
    EtaExpandApps -> strEtaExpandApps
    Identity -> strIdentity
    RemoveTypeArgs -> strRemoveTypeArgs
    MoveApps -> strMoveApps
    NatToPrimInt -> strNatToPrimInt
    IntToPrimInt -> strIntToPrimInt
    ConvertBuiltinTypes -> strConvertBuiltinTypes
    ComputeTypeInfo -> strComputeTypeInfo
    UnrollRecursion -> strUnrollRecursion
    DisambiguateNames -> strDisambiguateNames
    CombineInfoTables -> strCombineInfoTables
    CheckGeb -> strCheckGeb
    CheckExec -> strCheckExec
    CheckVampIR -> strCheckVampIR
    Normalize -> strNormalize
    LetFolding -> strLetFolding
    LambdaFolding -> strLambdaFolding
    LetHoisting -> strLetHoisting
    Inlining -> strInlining
    MandatoryInlining -> strMandatoryInlining
    FoldTypeSynonyms -> strFoldTypeSynonyms
    CaseCallLifting -> strCaseCallLifting
    SimplifyIfs -> strSimplifyIfs
    SimplifyComparisons -> strSimplifyComparisons
    SpecializeArgs -> strSpecializeArgs
    CaseFolding -> strCaseFolding
    CasePermutation -> strCasePermutation
    FilterUnreachable -> strFilterUnreachable
    OptPhaseEval -> strOptPhaseEval
    OptPhaseExec -> strOptPhaseExec
    OptPhaseGeb -> strOptPhaseGeb
    OptPhaseVampIR -> strOptPhaseVampIR
    OptPhaseMain -> strOptPhaseMain

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineStored -> strStoredPipeline
    PipelineNormalize -> strNormalizePipeline
    PipelineGeb -> strGebPipeline
    PipelineVampIR -> strVampIRPipeline
    PipelineStripped -> strStrippedPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineStored -> toStoredTransformations
    PipelineNormalize -> toNormalizeTransformations
    PipelineGeb -> toGebTransformations
    PipelineVampIR -> toVampIRTransformations
    PipelineStripped -> toStrippedTransformations
