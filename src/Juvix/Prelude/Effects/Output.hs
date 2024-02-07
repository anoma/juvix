module Juvix.Prelude.Effects.Output where

import Data.Kind qualified as GHC
import Juvix.Prelude.Base (fst, mapM_, return, reverse, (<$>))
import Juvix.Prelude.Effects.Base

data Output (o :: GHC.Type) :: Effect

type instance DispatchOf (Output _) = 'Static 'NoSideEffects

newtype instance StaticRep (Output o) = Output [o]

runOutputEff :: (o -> Eff r ()) -> Eff (Output o ': r) a -> Eff r a
runOutputEff handle m = do
  (ls, a) <- runOutputList m
  mapM_ handle ls
  return a

runOutputList :: Eff (Output o ': r) a -> Eff r ([o], a)
runOutputList m = do
  (a, Output s) <- runStaticRep (Output []) m
  return (reverse s, a)

ignoreOutput :: Eff (Output o ': r) a -> Eff r (a)
ignoreOutput m = fst <$> runStaticRep (Output []) m

output :: (Output o :> r) => o -> Eff r ()
output o = overStaticRep (\(Output l) -> Output (o : l))
