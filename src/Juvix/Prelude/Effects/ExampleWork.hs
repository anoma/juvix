module Juvix.Prelude.Effects.ExampleWork where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import Prelude

data MyEffect :: Effect where
  ReadSomething :: MyEffect m String
  Region :: m () -> MyEffect m Int

makeEffect ''MyEffect

runMyEffectSimple ::
  forall r a.
  Eff (MyEffect ': r) a ->
  Eff r (a, Int)
runMyEffectSimple = reinterpret @MyEffect @(State Int ': r) @a (runState 0) handler
  where
    handler :: forall t. EffectHandler MyEffect (State Int ': t)
    handler (localEnv :: LocalEnv localEs (State Int ': t)) = \case
      ReadSomething -> do
        modify @Int succ
        return "hello"
      Region (m :: Eff localEs ()) -> localSeqUnlift localEnv $ \unlift -> do
        unlift (impose (execState 0) handler m)

example :: IO ()
example = runEff . fmap fst . runMyEffectSimple $ do
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
