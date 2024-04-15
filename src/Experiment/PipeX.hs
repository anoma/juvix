{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-operator-whitespace #-}

module Experiment.PipeX where

import Data.List.Singletons
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Experiment.Stage
import Juvix.Prelude

type StageType :: Stage -> GHCType
type family StageType s = res where
  StageType 'Entry = ()
  StageType 'RawText = Text
  StageType 'RawString = String
  StageType 'Parsed = NonEmpty Text
  StageType 'Computed = Natural

data Options = Options

data Err = Err
  deriving stock (Show)

type Never = () ~ [()]

type StepContext :: Stage -> Stage -> [Effect] -> GHCConstraint
type family StepContext from to r = res where
  StepContext 'Entry 'RawText r = Members '[IOE] r
  StepContext 'Entry 'RawString r = Members '[IOE] r
  StepContext 'Entry _ _ = Never
  StepContext 'RawString 'RawText _ = ()
  StepContext 'RawString _ _ = Never
  StepContext 'RawText 'Parsed r = Members '[Reader Options, Error Err] r
  StepContext 'RawText _ _ = Never
  StepContext 'Parsed 'Computed r = Members '[Reader Options, State Int] r
  StepContext 'Parsed _ _ = Never
  StepContext 'Computed _ _ = Never

type PipelineContext :: Pipeline -> [Effect] -> GHCConstraint
type family PipelineContext ss r where
  PipelineContext '[] _ = Never
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

-- | TODO this is quadratic. Can it be improved?
type CanonicalPipeline :: Stage -> Stage -> Pipeline
type family CanonicalPipeline s1 s2 where
  CanonicalPipeline 'Entry 'RawText = '[ 'Entry, 'RawText ]
  CanonicalPipeline 'Entry 'Parsed = 'Entry ': CanonicalPipeline 'RawText 'Parsed
  CanonicalPipeline 'Entry 'Computed = 'Entry ': CanonicalPipeline 'RawText 'Computed
  CanonicalPipeline 'RawString 'Computed = 'RawString ': CanonicalPipeline 'RawText 'Computed
  CanonicalPipeline 'RawText 'Computed = 'RawText ': CanonicalPipeline 'Parsed 'Computed
  CanonicalPipeline 'RawText 'Parsed = '[ 'RawText, 'Parsed]
  CanonicalPipeline 'Parsed 'Computed = '[ 'Parsed, 'Computed]
  CanonicalPipeline x x = '[x]

-- | Canonical pipeline
pipeline ::
  forall (s1 :: Stage) (s2 :: Stage) (r :: [Effect]).
  (PipelineContext (CanonicalPipeline s1 s2) r, SingI (CanonicalPipeline s1 s2)) =>
  SStage s1 ->
  SStage s2 ->
  From (CanonicalPipeline s1 s2) ->
  Sem r (To (CanonicalPipeline s1 s2))
pipeline _ _ = customPipeline (sing :: SPipeline (CanonicalPipeline s1 s2))

customPipeline ::
  forall (p :: Pipeline) (r :: [Effect]).
  (PipelineContext p r) =>
  SPipeline p ->
  From p ->
  Sem r (To p)
customPipeline p arg = case p of
  SCons _ SNil -> return arg
  SCons s1 (SCons s2 ss) -> pipelineStep s1 s2 arg >>= customPipeline (SCons s2 ss)

pipelineStep ::
  forall (s1 :: Stage) (s2 :: Stage) (r :: [Effect]).
  (StepContext s1 s2 r) =>
  SStage s1 ->
  SStage s2 ->
  StageType s1 ->
  Sem r (StageType s2)
pipelineStep s1 s2 arg =
  case (s1, s2) of
    (SRawString, SRawText) -> return (pack arg)
    (SEntry, SRawText) -> liftIO Text.getLine
    (SEntry, SRawString) -> liftIO (unpack <$> getLine)
    (SRawText, SParsed) -> parse arg
    (SParsed, SComputed) -> compute arg

parse :: (StepContext 'RawText 'Parsed r) => Text -> Sem r (NonEmpty Text)
parse txt = do
  void (ask @Options)
  maybe (throw Err) pure (nonEmpty (Text.words txt))

compute :: forall r. (StepContext 'Parsed 'Computed r) => NonEmpty Text -> Sem r Natural
compute = fmap sum . mapM go
  where
    go :: Text -> Sem r Natural
    go w = do
      modify @Int succ
      return (fromIntegral (Text.length w))

runPipelinePure :: Text -> Either Err (Int, Natural)
runPipelinePure =
  run
    . runError
    . runState 0
    . runReader Options
    . pipeline SRawText SComputed

runUpToParsed :: Text -> Either Err (NonEmpty Text)
runUpToParsed =
  run
    . runError
    . runReader Options
    . pipeline SRawText SParsed

runPipelineIO :: IO (Either Err (Int, Natural))
runPipelineIO =
  runM
  . runReader Options
  . runError
  . runState @Int 0
  . pipeline SEntry SComputed
  $ ()
