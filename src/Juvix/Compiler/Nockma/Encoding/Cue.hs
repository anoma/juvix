module Juvix.Compiler.Nockma.Encoding.Cue where

import Data.Bit as Bit
import Data.Bits
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

data CueState a = CueState
  { _cueStateCache :: HashMap Int (Term a)
  }

initCueState :: CueState a
initCueState =
  CueState
    { _cueStateCache = mempty
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

registerElementStart ::
  ( Members
      '[ BitReader,
         Reader CueEnv
       ]
      r
  ) =>
  Sem r a ->
  Sem r a
registerElementStart sem = do
  pos <- getCurrentPosition
  local (set cueEnvStartPos pos) sem

handleBitError :: (Member (Error a) r) => a -> Sem (Error BitReadError ': r) x -> Sem r x
handleBitError e = mapError @_ @_ @BitReadError (const e)

consumeLength :: forall r. (Members '[BitReader, Error ReadError] r) => Sem r Int
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

consumeInteger ::
  forall r.
  ( Members
      '[ BitReader,
         Error ReadError
       ]
      r
  ) =>
  ReadError ->
  Int ->
  Sem r Integer
consumeInteger e len
  | len == 0 = return 0
  | otherwise =
      vectorBitsToInteger
        . build
        . foldable
        <$> handleBitError e (nextBits len)

data JamTag = JamTagAtom | JamTagCell | JamTagBackref

consumeTag ::
  forall r.
  ( Members
      '[ BitReader,
         Error ReadError
       ]
      r
  ) =>
  Sem r JamTag
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

cacheCueTerm ::
  forall a r.
  ( Members
      '[ State (CueState a),
         Reader CueEnv
       ]
      r
  ) =>
  Term a ->
  Sem r ()
cacheCueTerm t = do
  pos <- asks (^. cueEnvStartPos)
  modify' (set (cueStateCache . at pos) (Just t))

lookupCueCache ::
  ( Members
      '[ Error ReadError,
         State (CueState a)
       ]
      r
  ) =>
  Int ->
  Sem r (Term a)
lookupCueCache pos =
  fromMaybeM
    (throw ReadErrorCacheMiss)
    (gets (^. cueStateCache . at pos))

cueToVector ::
  ( NockNatural a,
    Member (Error (ErrNockNatural a)) r
  ) =>
  Atom a ->
  Sem r (Bit.Vector Bit)
cueToVector a' = do
  n <- nockNatural a'
  return (integerToVectorBits @Integer (fromIntegral n))

cueFromBits ::
  forall a r.
  ( NockNatural a,
    Members
      '[ Error ReadError,
         Error (ErrNockNatural a)
       ]
      r
  ) =>
  Bit.Vector Bit ->
  Sem r (Term a)
cueFromBits v = evalBitReader v (evalState (initCueState @a) (runReader initCueEnv cueFromBitsSem))

cueFromBitsSem ::
  forall a r.
  ( NockNatural a,
    Members
      '[ BitReader,
         Error ReadError,
         State (CueState a),
         Reader CueEnv,
         Error (ErrNockNatural a)
       ]
      r
  ) =>
  Sem r (Term a)
cueFromBitsSem = registerElementStart $ do
  tag <- consumeTag
  case tag of
    JamTagAtom -> goAtom
    JamTagBackref -> goBackref
    JamTagCell -> goCell
  where
    goAtom :: Sem r (Term a)
    goAtom = do
      a <- TermAtom <$> consumeAtom
      cacheCueTerm a
      return a

    goBackref :: Sem r (Term a)
    goBackref = do
      a <- consumeNatAtom
      idx <- maybe (throw ReadErrorInvalidBackref) return (safeNaturalToInt (a ^. atom))
      lookupCueCache idx

    goCell :: Sem r (Term a)
    goCell = do
      _cellLeft <- cueFromBitsSem
      _cellRight <- cueFromBitsSem
      let cell = TermCell (Cell' {_cellInfo = emptyCellInfo, ..})
      cacheCueTerm cell
      return cell

    consumeNatAtom :: Sem r (Atom Natural)
    consumeNatAtom = do
      len <- consumeLength
      if
          | len == 0 -> return (Atom 0 emptyAtomInfo)
          | otherwise -> do
              a <- consumeInteger ReadErrorInvalidAtom len
              return (Atom (fromInteger a) emptyAtomInfo)

    consumeAtom :: Sem r (Atom a)
    consumeAtom = do
      len <- consumeLength
      if
          | len == 0 -> do
              z <- fromNatural @a 0
              return (Atom z emptyAtomInfo)
          | otherwise -> do
              a <- consumeInteger ReadErrorInvalidAtom len
              n <- fromNatural (fromInteger a)
              return (Atom n emptyAtomInfo)

cueAtom ::
  ( NockNatural a,
    Members
      '[ Error (ErrNockNatural a),
         Error ReadError
       ]
      r
  ) =>
  Atom a ->
  Sem r (Term a)
cueAtom a' = cueToVector a' >>= cueFromBits

cue ::
  ( NockNatural a,
    Members
      '[ Error (ErrNockNatural a),
         Error ReadError
       ]
      r
  ) =>
  Term a ->
  Sem r (Term a)
cue = \case
  TermAtom a -> cueAtom a
  TermCell {} -> throw ReadErrorExpectedAtom

cueJust :: Term Natural -> Either ReadError (Term Natural)
cueJust = run . runErrorWith @NockNaturalNaturalError (\__ -> error ("fail")) . runError @ReadError . cue
