module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | LetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToInt
  | ConvertBuiltinTypes
  | Identity
  | UnrollRecursion
  | ComputeTypeInfo
  | MatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CheckGeb
  deriving stock (Data, Bounded, Enum)

data PipelineId
  = PipelineEval
  | PipelineGeb
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

toStrippedTransformations :: [TransformationId]
toStrippedTransformations =
  toEvalTransformations ++ [LambdaLetRecLifting, TopEtaExpand, MoveApps, RemoveTypeArgs]

toGebTransformations :: [TransformationId]
toGebTransformations = toEvalTransformations ++ [UnrollRecursion, ComputeTypeInfo]

toEvalTransformations :: [TransformationId]
toEvalTransformations = [EtaExpandApps, MatchToCase, NatToInt, ConvertBuiltinTypes]

pipeline :: PipelineId -> [TransformationId]
pipeline = \case
  PipelineEval -> toEvalTransformations
  PipelineGeb -> toGebTransformations
  PipelineStripped -> toStrippedTransformations
