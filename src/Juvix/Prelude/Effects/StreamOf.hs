module Juvix.Prelude.Effects.StreamOf
  ( StreamOf,
    yield,
    runStreamOf,
    runStreamOfNaturals,
  )
where

import Data.Stream
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

data StreamOf (i :: GHCType) :: Effect

type instance DispatchOf (StreamOf _) = 'Static 'NoSideEffects

newtype instance StaticRep (StreamOf i) = StreamOf
  { _unStreamOf :: Stream i
  }

yield :: (Member (StreamOf i) r) => Sem r i
yield = stateStaticRep $ \case
  StreamOf (Cons i is) -> (i, StreamOf is)

runStreamOf :: Stream i -> Sem (StreamOf i ': r) a -> Sem r a
runStreamOf = evalStaticRep . StreamOf

runStreamOfNaturals :: Sem (StreamOf Natural ': r) a -> Sem r a
runStreamOfNaturals = runStreamOf allNaturals
