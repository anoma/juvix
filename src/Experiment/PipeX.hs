{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

module Experiment.PipeX where

import Data.List.Singletons
import Data.Text qualified as Text
import Experiment.Stage
import Juvix.Prelude

type StageType :: Stage -> GHCType
type family StageType s = res where
  StageType 'Concrete = Text
  StageType 'Parsed = NonEmpty Text
  StageType 'Computed = Natural

data Entry = Entry

data Err = Err
  deriving stock (Show)

type Impossible = () ~ [()]

type StepContext :: Stage -> Stage -> [Effect] -> GHCConstraint
type family StepContext from to r = res where
  StepContext 'Concrete 'Parsed r = Members '[Reader Entry, Error Err] r
  StepContext 'Concrete _ _ = Impossible
  StepContext 'Parsed 'Computed r = Members '[Reader Entry, State Int] r
  StepContext 'Parsed _ _ = Impossible
  StepContext 'Computed _ _ = Impossible

type PipelineContext :: Pipeline -> [Effect] -> GHCConstraint
type family PipelineContext ss r where
  PipelineContext '[] _ = Impossible
  PipelineContext '[_] _ = ()
  PipelineContext (fromStage ': toStage ': ss) r =
    (StepContext fromStage toStage r, PipelineContext (toStage ': ss) r)

type From :: Pipeline -> GHCType
type family From ss where
  From (from ': _) = StageType from

type To :: Pipeline -> GHCType
type family To ss where
  To '[res] = StageType res
  To (_ ': s ': ss) = To (s ': ss)

pipeline ::
  forall (p :: Pipeline) (r :: [Effect]).
  (PipelineContext p r) =>
  SPipeline p ->
  From p ->
  Sem r (To p)
pipeline p arg = case p of
  SCons _ SNil -> return arg
  SCons s1 (SCons s2 ss) -> pipelineStep s1 s2 arg >>= pipeline (SCons s2 ss)

pipelineStep ::
  forall (s1 :: Stage) (s2 :: Stage) (r :: [Effect]).
  (StepContext s1 s2 r) =>
  SStage s1 ->
  SStage s2 ->
  StageType s1 ->
  Sem r (StageType s2)
pipelineStep s1 s2 arg =
  case (s1, s2) of
    (SConcrete, SParsed) -> parseHelper s2 arg
    (SParsed, SComputed) -> compute arg

parseHelper ::
  forall (s :: Stage) r.
  (StepContext 'Concrete s r) =>
  SStage s ->
  Text ->
  Sem r (NonEmpty Text)
parseHelper si = case si of
  SParsed -> parse

parse :: (StepContext 'Concrete 'Parsed r) => Text -> Sem r (NonEmpty Text)
parse txt = do
  void (ask @Entry)
  maybe (throw Err) pure (nonEmpty (Text.words txt))

compute :: forall r. (StepContext 'Parsed 'Computed r) => NonEmpty Text -> Sem r Natural
compute = fmap sum . mapM go
  where
    go :: Text -> Sem r Natural
    go w = do
      modify @Int succ
      return (fromIntegral (Text.length w))

runPip :: Text -> Either Err (Int, Natural)
runPip =
  run
    . runError
    . runState 0
    . runReader Entry
    . pipeline (SConcrete >>> SParsed >>>| SComputed)
