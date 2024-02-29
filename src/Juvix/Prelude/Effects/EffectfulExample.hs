module Juvix.Prelude.Effects.EffectfulExample where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Juvix.Prelude.Effects.Base.Internal
import Prelude

data D :: Effect where
  D :: D m ()

data E :: Effect where
  MkE :: E m ()

runE :: (IOE :> es, D :> es) => Eff (E ': es) a -> Eff es a
runE = error "runE"

data F :: Effect where
  MkF :: F m ()

makeEffect ''D
makeEffect ''E
makeEffect ''F

runF :: (IOE :> es) => Eff (F ': es) a -> Eff es a
runF = error "runF"

runD :: forall es a. (IOE :> es) => Eff (D ': es) a -> Eff es a
runD = interpret handler
  where
    handler :: forall t. (IOE :> t) => EffectHandler D t
    handler env = \case
      D -> localSeqUnlift env $ \unlift -> do
        localSeqHandle @IOE env $ \withIOE -> do
          unlift . withIOE . runE $ pure ()

runD2 :: forall es a. (IOE :> es, F :> es) => Eff (D ': es) a -> Eff es a
runD2 = interpret handler
  where
    handler :: forall t. (IOE :> t, F :> t) => EffectHandler D t
    handler env = \case
      D -> localSeqUnlift env $ \unlift ->
        localSeqHandle2 @IOE env $ \env1 withIOE ->
          localSeqHandle @F env1 $ \withF -> do
            unlift . withIOE . withF $ do
              liftIO (putStrLn "hi")
              d
              mkF
