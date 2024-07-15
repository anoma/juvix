module Juvix.Compiler.Nockma.Encoding.Effect.BitReader where

import Data.Bit as Bit
import Data.Vector.Generic qualified as V
import Data.Vector.Unboxed qualified as U
import Effectful (Eff)
import Juvix.Prelude.Base

data BitReadError = BitReadErrorNoMoreBits

data BitReader :: Effect where
  -- makeSem ''BitReader has a type error if Sem is used instead of Eff
  NextBit :: (Member (Error BitReadError) r) => BitReader (Eff r) Bit
  ConsumeRemaining :: BitReader m (Bit.Vector Bit)
  GetCurrentPosition :: BitReader m Int

makeSem ''BitReader

nextBits :: (Members '[BitReader, Error BitReadError] r) => Int -> Sem r [Bit]
nextBits n = replicateM n nextBit

countBitsUntil :: forall r. (Members '[BitReader, Error BitReadError] r) => (Bit -> Bool) -> Sem r Int
countBitsUntil p = go 0
  where
    go :: Int -> Sem r Int
    go n = do
      b <- nextBit
      if
        | p b -> return n
        | otherwise -> go (n + 1)

countBitsUntilOne :: (Members '[BitReader, Error BitReadError] r) => Sem r Int
countBitsUntilOne = countBitsUntil (== Bit True)

data ReaderState = ReaderState
  { _readerStateBits :: Bit.Vector Bit,
    _readerStateCurrentPos :: Int
  }

makeLenses ''ReaderState

initReaderState :: Bit.Vector Bit -> ReaderState
initReaderState bits =
  ReaderState
    { _readerStateBits = bits,
      _readerStateCurrentPos = 0
    }

evalBitReader :: Bit.Vector Bit -> Sem (BitReader ': r) a -> Sem r a
evalBitReader v = evalState (initReaderState v) . re

re :: Sem (BitReader ': r) a -> Sem (State ReaderState ': r) a
re = interpretTopH $ \env -> \case
  GetCurrentPosition -> gets (^. readerStateCurrentPos)
  ConsumeRemaining -> do
    pos <- gets (^. readerStateCurrentPos)
    bits <- gets (^. readerStateBits)
    modify' (set readerStateCurrentPos (V.length bits)) >> return (V.drop pos bits)
  NextBit -> do
    bits <- gets (^. readerStateBits)
    pos <- gets (^. readerStateCurrentPos)
    case bits U.!? pos of
      Just b -> modify' (over readerStateCurrentPos (+ 1)) >> return b
      Nothing -> localSeqUnlift env $ \unlift -> unlift (throw BitReadErrorNoMoreBits)
