module Juvix.Compiler.Nockma.Encoding where

import Data.Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

integerToVectorBits :: Integer -> U.Vector Bit
integerToVectorBits = build . integerToBuilder

integerToBuilder :: Integer -> Builder Bit
integerToBuilder x
  | x == 0 = Builder.singleton (Bit False)
  | x < 0 = error "integerToVectorBits: negative integers are not supported in this implementation"
  | otherwise = unfoldBits x
  where
    unfoldBits :: Integer -> Builder Bit
    unfoldBits n
      | n == 0 = Builder.empty
      | otherwise = Builder.singleton (Bit (testBit n 0)) <> unfoldBits (n `shiftR` 1)

bitLength :: Integer -> Int
bitLength = \case
  0 -> 0
  n -> go n 0
    where
      go :: Integer -> Int -> Int
      go 0 acc = acc
      go x acc = go (x `shiftR` 1) (acc + 1)

vectorBitsToInteger :: U.Vector Bit -> Integer
vectorBitsToInteger = U.ifoldl' go 0
  where
    go :: Integer -> Int -> Bit -> Integer
    go acc idx (Bit b)
      | b = setBit acc idx
      | otherwise = acc

data JamState = JamState
  { _jamStateCurrent :: Int,
    _jamStateRefs :: HashMap (Term Natural) Int,
    _jamStateBuilder :: Builder Bit
  }

initState :: JamState
initState =
  JamState
    { _jamStateCurrent = 0,
      _jamStateRefs = mempty,
      _jamStateBuilder = mempty
    }

makeLenses ''JamState

writeBit :: (Member (State JamState) r) => Bit -> Sem r ()
writeBit b = modify (incrementCurrent . appendByte)
  where
    incrementCurrent :: JamState -> JamState
    incrementCurrent = over jamStateCurrent (+ 1)

    appendByte :: JamState -> JamState
    appendByte = over jamStateBuilder (<> Builder.singleton b)

writeOne :: (Member (State JamState) r) => Sem r ()
writeOne = writeBit (Bit True)

writeZero :: (Member (State JamState) r) => Sem r ()
writeZero = writeBit (Bit False)

writeInteger :: (Member (State JamState) r) => Integer -> Sem r ()
writeInteger i = modify (updateCurrent . updateBuilder)
  where
    iBuilder :: Builder Bit
    iBuilder = integerToBuilder i

    updateCurrent :: JamState -> JamState
    updateCurrent = over jamStateCurrent (+ (Builder.size iBuilder))

    updateBuilder :: JamState -> JamState
    updateBuilder = over jamStateBuilder (<> iBuilder)

saveTerm :: (Member (State JamState) r) => Term Natural -> Sem r ()
saveTerm t = do
  c <- gets (^. jamStateCurrent)
  modify (set (jamStateRefs . at t) (Just c))

mat :: (Member (State JamState) r) => Integer -> Sem r ()
mat i = do
  let a = bitLength i
      b = bitLength (fromIntegral a)
      below = b - 1
  writeInteger (1 `shiftL` b)
  writeInteger ((.&.) (fromIntegral a) ((1 `shiftL` below) - 1))
  writeInteger i

back :: (Member (State JamState) r) => Int -> Sem r ()
back ref = do
  writeOne
  writeOne
  mat (fromIntegral ref)

maybeCell :: Term a -> Maybe (Cell a)
maybeCell = \case
  TermCell c -> Just c
  TermAtom {} -> Nothing

jamR :: forall r. (Member (State JamState) r) => Term Natural -> Sem r ()
jamR t = do
  mDupe <- gets (^. jamStateRefs . at t)
  case t of
    TermCell c -> maybe (goCell c) back mDupe
    TermAtom a -> maybe (goAtom a) (goAtomDupe a) mDupe
  where
    goCell :: Cell Natural -> Sem r ()
    goCell c = do
      saveTerm (TermCell c)
      writeOne
      writeZero
      jamR (c ^. cellLeft)
      jamR (c ^. cellRight)

    goAtom :: Atom Natural -> Sem r ()
    goAtom a = do
      saveTerm (TermAtom a)
      writeZero
      mat (fromIntegral (a ^. atom))

    goAtomDupe :: Atom Natural -> Int -> Sem r ()
    goAtomDupe a dupe' = do
      let isize = bitLength (fromIntegral (a ^. atom))
          dsize = bitLength (fromIntegral dupe')
      if
          | isize < dsize -> do
              writeZero
              mat (fromIntegral (a ^. atom))
          | otherwise -> back dupe'

jam :: Term Natural -> Atom Natural
jam = (\i -> Atom @Natural i emptyAtomInfo) . fromInteger . vectorBitsToInteger . jamToVector

jamToVector :: Term Natural -> U.Vector Bit
jamToVector = build . jamToBuilder

jamToBuilder :: Term Natural -> Builder Bit
jamToBuilder =
  (^. jamStateBuilder)
    . run
    . execState initState
    . jamR

cue :: Atom Natural -> Term Natural
cue = cueFromBits . cueToVector

cueFromBits :: U.Vector Bit -> Term Natural
cueFromBits = undefined

cueToVector :: Atom Natural -> U.Vector Bit
cueToVector = integerToVectorBits . fromIntegral . (^. atom)
