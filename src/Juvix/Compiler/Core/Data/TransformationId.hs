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
  | ComputeCaseANF
  | DetectRedundantPatterns
  | MatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CombineInfoTables
  | CheckExec
  | CheckRust
  | CheckVampIR
  | CheckAnoma
  | CheckCairo
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
  | ConstantFolding
  | FilterUnreachable
  | OptPhaseEval
  | OptPhaseExec
  | OptPhaseVampIR
  | OptPhaseMain
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineStored
  | PipelineNormalize
  | PipelineVampIR
  | PipelineStripped
  | PipelineExec
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations = [DetectRedundantPatterns, MatchToCase]

toStoredTransformations :: [TransformationId]
toStoredTransformations = [EtaExpandApps, DetectRedundantPatterns, MatchToCase, NatToPrimInt, IntToPrimInt, ConvertBuiltinTypes, OptPhaseEval, DisambiguateNames]

combineInfoTablesTransformations :: [TransformationId]
combineInfoTablesTransformations = [CombineInfoTables, FilterUnreachable]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations = [CombineInfoTables, LetRecLifting, LetFolding, UnrollRecursion]

toVampIRTransformations :: [TransformationId]
toVampIRTransformations =
  combineInfoTablesTransformations ++ [CheckVampIR, LetRecLifting, OptPhaseVampIR, UnrollRecursion, Normalize, LetHoisting]

toStrippedTransformations :: TransformationId -> [TransformationId]
toStrippedTransformations checkId =
  combineInfoTablesTransformations ++ [checkId, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps, RemoveTypeArgs, DisambiguateNames]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    LambdaLetRecLifting -> strLifting
    LetRecLifting -> strLetRecLifting
    TopEtaExpand -> strTopEtaExpand
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
    ComputeCaseANF -> strComputeCaseANF
    UnrollRecursion -> strUnrollRecursion
    DisambiguateNames -> strDisambiguateNames
    CombineInfoTables -> strCombineInfoTables
    CheckExec -> strCheckExec
    CheckRust -> strCheckRust
    CheckVampIR -> strCheckVampIR
    CheckAnoma -> strCheckAnoma
    CheckCairo -> strCheckCairo
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
    ConstantFolding -> strConstantFolding
    FilterUnreachable -> strFilterUnreachable
    OptPhaseEval -> strOptPhaseEval
    OptPhaseExec -> strOptPhaseExec
    OptPhaseVampIR -> strOptPhaseVampIR
    OptPhaseMain -> strOptPhaseMain

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineStored -> strStoredPipeline
    PipelineNormalize -> strNormalizePipeline
    PipelineVampIR -> strVampIRPipeline
    PipelineStripped -> strStrippedPipeline
    PipelineExec -> strExecPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineStored -> toStoredTransformations
    PipelineNormalize -> toNormalizeTransformations
    PipelineVampIR -> toVampIRTransformations
    PipelineStripped -> toStrippedTransformations IdentityTrans
    PipelineExec -> toStrippedTransformations CheckExec
