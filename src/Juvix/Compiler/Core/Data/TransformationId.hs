module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | LetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToInt
  | IntToInt
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
  | LetFolding
  | FoldTypeSynonyms
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

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations = [MatchToCase]

toEvalTransformations :: [TransformationId]
toEvalTransformations = [EtaExpandApps, MatchToCase, NatToInt, IntToInt, ConvertBuiltinTypes, LetFolding]

toStrippedTransformations :: [TransformationId]
toStrippedTransformations =
  toEvalTransformations ++ [CheckExec, LambdaLetRecLifting, LetFolding, TopEtaExpand, MoveApps, RemoveTypeArgs]

toGebTransformations :: [TransformationId]
toGebTransformations = toEvalTransformations ++ [CheckGeb, LetRecLifting, LetFolding, UnrollRecursion, FoldTypeSynonyms, ComputeTypeInfo]

pipeline :: PipelineId -> [TransformationId]
pipeline = \case
  PipelineEval -> toEvalTransformations
  PipelineGeb -> toGebTransformations
  PipelineStripped -> toStrippedTransformations
