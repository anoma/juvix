module Juvix.Compiler.Core.Data.TransformationId where

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

data TransformationLikeId
  = TransformationId TransformationId
  | PipelineId PipelineId
  deriving stock (Data)

allTransformationLikeIds :: [TransformationLikeId]
allTransformationLikeIds =
  map TransformationId allElements
    ++ map PipelineId allElements

fromTransformationLike :: TransformationLikeId -> [TransformationId]
fromTransformationLike = \case
  TransformationId i -> [i]
  PipelineId p -> pipeline p

fromTransformationLikes :: [TransformationLikeId] -> [TransformationId]
fromTransformationLikes = concatMap fromTransformationLike

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
  [CombineInfoTables, FilterUnreachable, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps, RemoveTypeArgs]

toExec :: [TransformationId]
toExec = [CheckExec]

toGebTransformations :: [TransformationId]
toGebTransformations = [CombineInfoTables, FilterUnreachable, CheckGeb, LetRecLifting, OptPhaseGeb, UnrollRecursion, FoldTypeSynonyms, ComputeTypeInfo]

pipeline :: PipelineId -> [TransformationId]
pipeline = \case
  PipelineStored -> toStoredTransformations
  PipelineNormalize -> toNormalizeTransformations
  PipelineGeb -> toGebTransformations
  PipelineVampIR -> toVampIRTransformations
  PipelineStripped -> toStrippedTransformations
