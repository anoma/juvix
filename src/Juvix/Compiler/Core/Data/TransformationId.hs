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
  | CheckGeb
  | CheckExec
  | CheckVampIR
  | Normalize
  | LetFolding
  | LambdaFolding
  | LetHoisting
  | Inlining
  | FoldTypeSynonyms
  | CaseCallLifting
  | SimplifyIfs
  | SpecializeArgs
  | CaseFolding
  | OptPhaseEval
  | OptPhaseExec
  | OptPhaseGeb
  | OptPhaseVampIR
  | OptPhaseMain
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineEval
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

toEvalTransformations :: [TransformationId]
toEvalTransformations = [EtaExpandApps, MatchToCase, NatToPrimInt, IntToPrimInt, ConvertBuiltinTypes, OptPhaseEval]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations = toEvalTransformations ++ [LetRecLifting, LetFolding, UnrollRecursion]

toVampIRTransformations :: [TransformationId]
toVampIRTransformations = toEvalTransformations ++ [CheckVampIR, LetRecLifting, OptPhaseVampIR, UnrollRecursion, Normalize, LetHoisting]

toStrippedTransformations :: [TransformationId]
toStrippedTransformations =
  toEvalTransformations ++ [CheckExec, LambdaLetRecLifting, OptPhaseExec, TopEtaExpand, MoveApps, RemoveTypeArgs]

toGebTransformations :: [TransformationId]
toGebTransformations = toEvalTransformations ++ [CheckGeb, LetRecLifting, OptPhaseGeb, UnrollRecursion, FoldTypeSynonyms, ComputeTypeInfo]

pipeline :: PipelineId -> [TransformationId]
pipeline = \case
  PipelineEval -> toEvalTransformations
  PipelineNormalize -> toNormalizeTransformations
  PipelineGeb -> toGebTransformations
  PipelineVampIR -> toVampIRTransformations
  PipelineStripped -> toStrippedTransformations
