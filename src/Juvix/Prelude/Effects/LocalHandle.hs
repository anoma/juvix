module Juvix.Prelude.Effects.LocalHandle where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base.Internal (localSeqHandle)

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

runMyEffect :: (Fs :> es) => Eff (MyEffect ': es) a -> Eff es (a, Int)
runMyEffect = reinterpret (runState 0) handler
  where
    handler :: forall t. (Fs :> t) => EffectHandler MyEffect (State Int ': t)
    handler env = \case
      ReadSomething -> localSeqUnlift env $ \unlift -> do
        localSeqHandle @Fs env $ \withFs -> do
          modify @Int succ
          unlift . withFs . runFs $ pure "hello"
      Region m -> localSeqUnlift env $ \unlift -> do
        localSeqHandle @Fs env $ \(withFs :: Eff (Fs ': localEs) Int -> Eff localEs Int) -> do
          unlift . withFs $ impose (execState 0) handler (raise m)

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
