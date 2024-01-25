{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Compiler.Core.Data.TransformationId.Base where

import Juvix.Prelude

class TransformationId' t where
  transformationText :: t -> Text

class (Enum p, Enum t, Bounded p, Bounded t, TransformationId' t) => PipelineId' t p | p -> t where
  pipelineText :: p -> Text
  pipeline :: p -> [t]

data TransformationLikeId' t p
  = TransformationId t
  | PipelineId p
  deriving stock (Data)

allTransformationLikeIds ::
  (Bounded t, Bounded p, Enum t, Enum p) =>
  [TransformationLikeId' t p]
allTransformationLikeIds =
  map TransformationId allElements
    ++ map PipelineId allElements

fromTransformationLike :: (PipelineId' t p) => TransformationLikeId' t p -> [t]
fromTransformationLike = \case
  TransformationId i -> [i]
  PipelineId p -> pipeline p

fromTransformationLikes :: (PipelineId' t p) => [TransformationLikeId' t p] -> [t]
fromTransformationLikes = concatMap fromTransformationLike
