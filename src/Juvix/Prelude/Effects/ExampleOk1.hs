module Juvix.Prelude.Effects.ExampleOk1 where


import Juvix.Prelude

data MyEffect :: Effect where
  ReadSomething :: MyEffect m Text
  Region :: m a -> MyEffect m a


data Fs :: Effect where
  UseFs :: Fs m Text

makeSem ''MyEffect
makeSem ''Fs

runMyEffect :: forall r a. Member Fs r =>
  Sem (MyEffect ': r) a -> Sem r  a
runMyEffect = interpretH handler
  where
    handler :: forall x localEs.
        LocalEnv localEs r
        -> MyEffect (Sem localEs) x
        -> Sem r x
    handler localEnv = \case
      ReadSomething -> useFs
      Region m -> localSeqUnlift localEnv $ \unlift ->
        unlift m
