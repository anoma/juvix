module Juvix.Prelude.Effects.Accum where

import Data.Kind qualified as GHC
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

data Accum (o :: GHC.Type) :: Effect

type instance DispatchOf (Accum _) = 'Static 'NoSideEffects

newtype instance StaticRep (Accum o) = Accum
  { _unAccum :: [o]
  }

runAccumList :: Eff (Accum o ': r) a -> Eff r ([o], a)
runAccumList m = do
  (a, Accum s) <- runStaticRep (Accum mempty) m
  return (reverse s, a)

execAccumList :: Eff (Accum o ': r) a -> Eff r [o]
execAccumList = fmap fst . runAccumList

ignoreAccum :: Eff (Accum o ': r) a -> Eff r a
ignoreAccum m = snd <$> runAccumList m

accum :: (Accum o :> r) => o -> Eff r ()
accum o = overStaticRep (\(Accum l) -> Accum (o : l))
