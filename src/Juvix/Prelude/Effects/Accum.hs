module Juvix.Prelude.Effects.Accum
  ( Accum,
    runAccumList,
    execAccumList,
    ignoreAccum,
    accum,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

data Accum (o :: GHCType) :: Effect

type instance DispatchOf (Accum _) = 'Static 'NoSideEffects

newtype instance StaticRep (Accum o) = Accum
  { _unAccum :: [o]
  }

accum :: (Member (Accum o) r) => o -> Sem r ()
accum o = overStaticRep (\(Accum l) -> Accum (o : l))

runAccumList :: Sem (Accum o ': r) a -> Sem r ([o], a)
runAccumList m = do
  (a, Accum s) <- runStaticRep (Accum mempty) m
  return (reverse s, a)

execAccumList :: Sem (Accum o ': r) a -> Sem r [o]
execAccumList = fmap fst . runAccumList

ignoreAccum :: Sem (Accum o ': r) a -> Sem r a
ignoreAccum m = snd <$> runAccumList m
