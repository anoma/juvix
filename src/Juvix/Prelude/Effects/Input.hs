{-# LANGUAGE AllowAmbiguousTypes #-}

module Juvix.Prelude.Effects.Input
  ( Input,
    input,
    inputJust,
    inputWhile,
    peekInput,
    runInputList,
    repeatOnInput,
    isEndOfInput,
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
  stateStaticRep $
    \case
      Input [] -> (Nothing, Input [])
      Input (i : is) -> (Just i, Input is)

inputWhile :: forall i r. (Member (Input i) r) => (i -> Bool) -> Sem r [i]
inputWhile c =
  stateStaticRep $
    \case
      Input l ->
        let (sat, rest) = span c l
         in (sat, Input rest)

peekInput :: (Member (Input i) r) => Sem r (Maybe i)
peekInput = do
  Input l <- getStaticRep
  return (headMay l)

runInputList :: [i] -> Sem (Input i ': r) a -> Sem r a
runInputList = evalStaticRep . Input

isEndOfInput :: forall i r. (Members '[Input i] r) => Sem r Bool
isEndOfInput = isNothing <$> peekInput @i

repeatOnInput :: (Members '[Input i] r) => (i -> Sem r a) -> Sem r ()
repeatOnInput m = whenJustM input (m >=> const (repeatOnInput m))

inputJust :: (Members '[Input i] r) => Sem r i
inputJust = fromMaybe (error "inputJust") <$> input
