module Juvix.Compiler.Nockma.Encoding.Cue where

import Data.Bit as Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader

data CueState = CueState
  {_cueStateCache :: HashMap Int (Term Natural)
  }

initCueState :: CueState
initCueState =
  CueState
    {_cueStateCache = mempty
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

registerElementStart :: (Members '[BitReader, State CueState, Reader CueEnv] r) => Sem r a -> Sem r a
registerElementStart sem = do
  pos <- getCurrentPosition
  local (set cueEnvStartPos pos) sem

handleBitError :: (Member (Error a) r) => a -> Sem (Error BitReadError ': r) x -> Sem r x
handleBitError e = mapError @_ @_ @BitReadError (const e)

consumeLength :: forall r. (Members '[BitReader, Error ReadError, State CueState] r) => Sem r Int
consumeLength = do
  lenOfLen <- handleBitError ReadErrorInvalidLength countBitsUntilOne
  if
      | lenOfLen == 0 -> return 0
      | otherwise -> do
          -- The mist significant bit of the length is omitted
          let lenBits = lenOfLen - 1
          foldlM go (bit lenBits) [0 .. lenBits - 1]
  where
    go :: Int -> Int -> Sem r Int
    go acc n = do
      Bit b <- handleBitError ReadErrorInvalidLength nextBit
      return $
        if
            | b -> setBit acc n
            | otherwise -> acc

consumeInteger :: forall r. (Members '[BitReader, Error ReadError, State CueState] r) => ReadError -> Int -> Sem r Integer
consumeInteger e len
  | len == 0 = return 0
  | otherwise =
      vectorBitsToInteger
        . build
        . foldable
        <$> handleBitError e (nextBits len)

data JamTag = JamTagAtom | JamTagCell | JamTagBackref

consumeTag :: forall r. (Members '[BitReader, Error ReadError, State CueState] r) => Sem r JamTag
consumeTag = do
  Bit b0 <- nextBit'
  if
      | b0 -> do
          Bit b1 <- nextBit'
          if
              | b1 -> return JamTagBackref
              | otherwise -> return JamTagCell
      | otherwise -> return JamTagAtom

    where
      nextBit' :: Sem r Bit
      nextBit' = handleBitError ReadErrorInvalidTag nextBit

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
cueToVector = integerToVectorBits @Integer . fromIntegral . (^. atom)

cueFromBits :: (Member (Error ReadError) r) => Bit.Vector Bit -> Sem r (Term Natural)
cueFromBits v = evalBitReader v (evalState initCueState (runReader initCueEnv cueFromBitsSem))

cueFromBitsSem :: forall r. (Members '[BitReader, Error ReadError, State CueState, Reader CueEnv] r) => Sem r (Term Natural)
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
