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
  | NaiveMatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CheckGeb
  | LetFolding
  deriving stock (Data, Bounded, Enum, Show)

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

toEvalTransformations :: [TransformationId]
toEvalTransformations = [EtaExpandApps, MatchToCase, NatToInt, ConvertBuiltinTypes, LetFolding]

toStrippedTransformations :: [TransformationId]
toStrippedTransformations =
  toEvalTransformations ++ [LambdaLetRecLifting, TopEtaExpand, MoveApps, RemoveTypeArgs]

toGebTransformations :: [TransformationId]
toGebTransformations = toEvalTransformations ++ [LetRecLifting, CheckGeb, UnrollRecursion, ComputeTypeInfo]

pipeline :: PipelineId -> [TransformationId]
pipeline = \case
  PipelineEval -> toEvalTransformations
  PipelineGeb -> toGebTransformations
  PipelineStripped -> toStrippedTransformations
