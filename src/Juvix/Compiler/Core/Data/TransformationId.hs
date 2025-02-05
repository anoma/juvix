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
  | IdentityTrans
  | UnrollRecursion
  | ComputeTypeInfo
  | DetectConstantSideConditions
  | DetectRedundantPatterns
  | MatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CombineInfoTables
  | CheckExec
  | CheckRust
  | CheckAnoma
  | CheckCairo
  | Normalize
  | LetFolding
  | LambdaFolding
  | LoopHoisting
  | Inlining
  | MandatoryInlining
  | SimplifyIfs
  | SimplifyComparisons
  | SpecializeArgs
  | CaseFolding
  | CasePermutation
  | ConstantFolding
  | FilterUnreachable
  | OptPhaseEval
  | OptPhaseExec
  | OptPhaseMain
  | OptPhasePreLifting
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineStored
  | PipelineNormalize
  | PipelineStripped
  | PipelineExec
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations = [DetectConstantSideConditions, DetectRedundantPatterns, MatchToCase]

toStoredTransformations :: [TransformationId]
toStoredTransformations = [EtaExpandApps, DetectConstantSideConditions, DetectRedundantPatterns, MatchToCase, NatToPrimInt, IntToPrimInt, ConvertBuiltinTypes, OptPhaseEval, DisambiguateNames, OptPhasePreLifting, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps]

combineInfoTablesTransformations :: [TransformationId]
combineInfoTablesTransformations = [CombineInfoTables, FilterUnreachable]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations = [CombineInfoTables, LetRecLifting, LetFolding, UnrollRecursion]

toStrippedTransformations :: TransformationId -> [TransformationId]
toStrippedTransformations checkId =
  combineInfoTablesTransformations ++ [checkId, RemoveTypeArgs, DisambiguateNames]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    LambdaLetRecLifting -> strLifting
    LetRecLifting -> strLetRecLifting
    TopEtaExpand -> strTopEtaExpand
    DetectConstantSideConditions -> strDetectConstantSideConditions
    DetectRedundantPatterns -> strDetectRedundantPatterns
    MatchToCase -> strMatchToCase
    EtaExpandApps -> strEtaExpandApps
    IdentityTrans -> strIdentity
    RemoveTypeArgs -> strRemoveTypeArgs
    MoveApps -> strMoveApps
    NatToPrimInt -> strNatToPrimInt
    IntToPrimInt -> strIntToPrimInt
    ConvertBuiltinTypes -> strConvertBuiltinTypes
    ComputeTypeInfo -> strComputeTypeInfo
    UnrollRecursion -> strUnrollRecursion
    DisambiguateNames -> strDisambiguateNames
    CombineInfoTables -> strCombineInfoTables
    CheckExec -> strCheckExec
    CheckRust -> strCheckRust
    CheckAnoma -> strCheckAnoma
    CheckCairo -> strCheckCairo
    Normalize -> strNormalize
    LetFolding -> strLetFolding
    LambdaFolding -> strLambdaFolding
    LoopHoisting -> strLoopHoisting
    Inlining -> strInlining
    MandatoryInlining -> strMandatoryInlining
    SimplifyIfs -> strSimplifyIfs
    SimplifyComparisons -> strSimplifyComparisons
    SpecializeArgs -> strSpecializeArgs
    CaseFolding -> strCaseFolding
    CasePermutation -> strCasePermutation
    ConstantFolding -> strConstantFolding
    FilterUnreachable -> strFilterUnreachable
    OptPhaseEval -> strOptPhaseEval
    OptPhaseExec -> strOptPhaseExec
    OptPhaseMain -> strOptPhaseMain
    OptPhasePreLifting -> strOptPhasePreLifting

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineStored -> strStoredPipeline
    PipelineNormalize -> strNormalizePipeline
    PipelineStripped -> strStrippedPipeline
    PipelineExec -> strExecPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineStored -> toStoredTransformations
    PipelineNormalize -> toNormalizeTransformations
    PipelineStripped -> toStrippedTransformations IdentityTrans
    PipelineExec -> toStrippedTransformations CheckExec
