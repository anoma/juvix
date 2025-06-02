module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Core.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | LetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | RemoveInductiveParams
  | ResolveExterns
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
  | Trace
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineEval
  | PipelineExec
  | PipelineTypecheck
  | PipelineNormalize
  | PipelineStripped
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations =
  [ EtaExpandApps,
    DetectConstantSideConditions,
    DetectRedundantPatterns,
    MatchToCase,
    ResolveExterns
  ]

toEvalTransformations :: [TransformationId]
toEvalTransformations =
  [ EtaExpandApps,
    DetectConstantSideConditions,
    DetectRedundantPatterns,
    MatchToCase,
    NatToPrimInt,
    IntToPrimInt,
    ConvertBuiltinTypes,
    OptPhaseEval,
    DisambiguateNames
  ]

toExecTransformations :: [TransformationId]
toExecTransformations =
  toEvalTransformations ++ [ResolveExterns, OptPhasePreLifting, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations =
  toEvalTransformations ++ [CombineInfoTables, LetRecLifting, LetFolding, UnrollRecursion]

toStrippedTransformations0 :: TransformationId -> [TransformationId]
toStrippedTransformations0 checkId =
  [FilterUnreachable, checkId, RemoveTypeArgs]

-- | The `toStrippedTransformations` need to be broken into two parts for the
-- modular pipeline. The probelm is that `RemoveTypeArgs` modifies the types of
-- inductives, changing their arity. The `RemoveInductiveParams` would query old
-- inductive arities if they are from a different module. Breaking the stripped
-- transformation pipeline after `RemoveTypeArgs` ensures that all modules are
-- processed up to that point and `RemoveInductiveParams` uses new inductive
-- arities.
toStrippedTransformations1 :: [TransformationId]
toStrippedTransformations1 =
  [RemoveInductiveParams, DisambiguateNames]

toStrippedTransformations :: TransformationId -> [TransformationId]
toStrippedTransformations checkId =
  toStrippedTransformations0 checkId ++ toStrippedTransformations1

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
    RemoveInductiveParams -> strRemoveInductiveParams
    ResolveExterns -> strResolveExterns
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
    Trace -> strTrace

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineEval -> strEvalPipeline
    PipelineExec -> strExecPipeline
    PipelineTypecheck -> strTypecheckPipeline
    PipelineNormalize -> strNormalizePipeline
    PipelineStripped -> strStrippedPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineEval -> toEvalTransformations
    PipelineExec -> toExecTransformations
    PipelineTypecheck -> toTypecheckTransformations
    PipelineNormalize -> toNormalizeTransformations
    PipelineStripped -> toStrippedTransformations IdentityTrans
