module Juvix.Prelude.Effects.ExampleBad1 where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import Prelude

data MyEffect :: Effect where
  ReadSomething :: MyEffect m String
  Region :: m () -> MyEffect m Int

data Fs :: Effect where
  UseFs :: Fs m String

makeEffect ''MyEffect
makeEffect ''Fs

runFs :: Eff (Fs ': r) a -> Eff r a
runFs = interpret $ \_ -> \case
  UseFs -> return "hello"

runMyEffect ::
  forall r a.
  (Fs :> r) =>
  Eff (MyEffect ': r) a ->
  Eff r (a, Int)
runMyEffect = reinterpret @MyEffect @(State Int ': r) @a (runState 0) handler
  where
    handler :: forall t. (Fs :> t) => EffectHandler MyEffect (State Int ': t)
    handler (localEnv :: LocalEnv localEs (State Int ': t)) = \case
      ReadSomething -> do
        modify @Int succ
        useFs
      Region (m :: Eff localEs ()) -> localSeqUnlift localEnv $ \unlift -> do
        unlift (impose (execState 0) handler m)

example :: IO ()
example = runEff . runFs . fmap fst . runMyEffect $ do
  x <- region $ do
    y <- region $ do
      readSomething
      return ()
    liftIO (print y)
    readSomething
    readSomething
    readSomething
    readSomething
    return ()
  liftIO (print x)
  return ()
