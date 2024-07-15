module Juvix.Prelude.Effects.Input
  ( Input,
    input,
    inputJust,
    peekInput,
    runInputList,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base
import Safe

data Input (i :: GHCType) :: Effect

type instance DispatchOf (Input _) = 'Static 'NoSideEffects

newtype instance StaticRep (Input i) = Input
  { _unInput :: [i]
  }

input :: (Member (Input i) r) => Sem r (Maybe i)
input =
  stateStaticRep
    $ \case
      Input [] -> (Nothing, Input [])
      Input (i : is) -> (Just i, Input is)

peekInput :: (Member (Input i) r) => Sem r (Maybe i)
peekInput = do
  Input l <- getStaticRep
  return (headMay l)

runInputList :: [i] -> Sem (Input i ': r) a -> Sem r a
runInputList = evalStaticRep . Input

inputJust :: (Members '[Input i] r) => Sem r i
inputJust = fromMaybe (error "inputJust") <$> input
