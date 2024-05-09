module Juvix.Compiler.Nockma.Encoding.Cue where

import Data.Bit as Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

data CueState = CueState
  { _cueStateBits :: Bit.Vector Bit,
    _cueStateBitPos :: Int,
    _cueStateCache :: HashMap Int (Term Natural)
  }

initCueState :: Bit.Vector Bit -> CueState
initCueState v =
  CueState
    { _cueStateBits = v,
      _cueStateBitPos = 0,
      _cueStateCache = mempty
    }

data CueEnv = CueEnv
  {_cueEnvStartPos :: Int}

initCueEnv :: CueEnv
initCueEnv = CueEnv {_cueEnvStartPos = 0}

makeLenses ''CueState
makeLenses ''CueEnv

data ReadError
  = ReadErrorEnd
  | ReadErrorInvalidTag
  | ReadErrorCacheMiss
  | ReadErrorInvalidLength
  | ReadErrorExpectedAtom
  | ReadErrorInvalidAtom
  | ReadErrorInvalidBackref
  deriving stock (Show)

consumeBit :: (Members '[Error ReadError, State CueState] r) => ReadError -> Sem r Bit
consumeBit e = do
  bits <- gets (^. cueStateBits)
  pos <- gets (^. cueStateBitPos)
  nextBit <- maybe (throw e) return (bits U.!? pos)
  modify' (over cueStateBitPos (+ 1))
  return nextBit

registerElementStart :: (Members '[State CueState, Reader CueEnv] r) => Sem r a -> Sem r a
registerElementStart sem = do
  pos <- gets (^. cueStateBitPos)
  local (set cueEnvStartPos pos) sem

consumeBits :: (Members '[Error ReadError, State CueState] r) => ReadError -> Int -> Sem r [Bit]
consumeBits e n = replicateM n (consumeBit e)

countBitsUntil :: forall r. (Members '[Error ReadError, State CueState] r) => ReadError -> (Bit -> Bool) -> Sem r Int
countBitsUntil e p = go 0
  where
    go :: Int -> Sem r Int
    go n = do
      b <- consumeBit e
      if
          | p b -> return n
          | otherwise -> go (n + 1)

countBitsUntilOne :: forall r. (Members '[Error ReadError, State CueState] r) => ReadError -> Sem r Int
countBitsUntilOne e = countBitsUntil e (== Bit True)

consumeLength :: forall r. (Members '[Error ReadError, State CueState] r) => Sem r Int
consumeLength = do
  lenOfLen <- countBitsUntilOne ReadErrorInvalidLength

  if
      | lenOfLen == 0 -> return 0
      | otherwise -> do
          -- The mist significant bit of the length is omitted
          let lenBits = lenOfLen - 1
          foldlM go (bit lenBits) [0 .. lenBits - 1]
  where
    go :: Int -> Int -> Sem r Int
    go acc n = do
      Bit b <- consumeBit ReadErrorInvalidLength
      return $
        if
            | b -> setBit acc n
            | otherwise -> acc

consumeInteger :: forall r. (Members '[Error ReadError, State CueState] r) => ReadError -> Int -> Sem r Integer
consumeInteger e len
  | len == 0 = return 0
  | otherwise =
      vectorBitsToInteger
        . build
        . foldable
        <$> consumeBits e len

data JamTag = JamTagAtom | JamTagCell | JamTagBackref

consumeTag :: forall r. (Members '[Error ReadError, State CueState] r) => Sem r JamTag
consumeTag = do
  Bit b0 <- consumeBit ReadErrorEnd
  if
      | b0 -> do
          Bit b1 <- consumeBit ReadErrorEnd
          if
              | b1 -> return JamTagBackref
              | otherwise -> return JamTagCell
      | otherwise -> return JamTagAtom

cacheCueTerm :: (Members '[State CueState, Reader CueEnv] r) => Term Natural -> Sem r ()
cacheCueTerm t = do
  pos <- asks (^. cueEnvStartPos)
  modify' (set (cueStateCache . at pos) (Just t))

lookupCueCache :: (Members '[Error ReadError, State CueState] r) => Int -> Sem r (Term Natural)
lookupCueCache pos =
  fromMaybeM
    (throw ReadErrorCacheMiss)
    (gets (^. cueStateCache . at pos))

cueToVector :: Atom Natural -> Bit.Vector Bit
cueToVector = integerToVectorBits . fromIntegral . (^. atom)

cueFromBits :: (Member (Error ReadError) r) => Bit.Vector Bit -> Sem r (Term Natural)
cueFromBits v = evalState (initCueState v) (runReader initCueEnv cueFromBitsSem)

cueFromBitsSem :: forall r. (Members '[Error ReadError, State CueState, Reader CueEnv] r) => Sem r (Term Natural)
cueFromBitsSem = registerElementStart $ do
  tag <- consumeTag
  case tag of
    JamTagAtom -> goAtom
    JamTagBackref -> goBackref
    JamTagCell -> goCell
  where
    goAtom :: Sem r (Term Natural)
    goAtom = do
      a <- TermAtom <$> consumeAtom
      cacheCueTerm a
      return a

    goBackref :: Sem r (Term Natural)
    goBackref = do
      a <- consumeAtom
      idx <- maybe (throw ReadErrorInvalidBackref) return (safeNaturalToInt (a ^. atom))
      lookupCueCache idx

    goCell :: Sem r (Term Natural)
    goCell = do
      _cellLeft <- cueFromBitsSem
      _cellRight <- cueFromBitsSem
      let cell = TermCell (Cell' {_cellInfo = emptyCellInfo, ..})
      cacheCueTerm cell
      return cell

    consumeAtom :: Sem r (Atom Natural)
    consumeAtom = do
      len <- consumeLength
      if
          | len == 0 -> return (Atom 0 emptyAtomInfo)
          | otherwise -> do
              a <- consumeInteger ReadErrorInvalidAtom len
              return (Atom (fromInteger a) emptyAtomInfo)

cueAtom :: (Member (Error ReadError) r) => Atom Natural -> Sem r (Term Natural)
cueAtom = cueFromBits . cueToVector

cue :: (Member (Error ReadError) r) => Term Natural -> Sem r (Term Natural)
cue = \case
  TermAtom a -> cueAtom a
  TermCell {} -> throw ReadErrorExpectedAtom

cueJust :: Term Natural -> Either ReadError (Term Natural)
cueJust = run . runError @ReadError . cue
