module Juvix.Compiler.Nockma.Encoding where

import Data.Bit as Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

integerToVectorBits :: Integer -> Bit.Vector Bit
integerToVectorBits = build . integerToBuilder

integerToBuilder :: (Integral a) => a -> Builder Bit
integerToBuilder x
  | x < 0 = error "integerToVectorBits: negative integers are not supported in this implementation"
  | otherwise = unfoldBits (fromIntegral x)
  where
    unfoldBits :: Integer -> Builder Bit
    unfoldBits n
      | n == 0 = Builder.empty
      | otherwise = Builder.singleton (Bit (testBit n 0)) <> unfoldBits (n `shiftR` 1)

bitLength :: forall a. (Integral a) => a -> Int
bitLength = \case
  0 -> 0
  n -> go (fromIntegral n) 0
    where
      go :: Integer -> Int -> Int
      go 0 acc = acc
      go x acc = go (x `shiftR` 1) (acc + 1)

vectorBitsToInteger :: Bit.Vector Bit -> Integer
vectorBitsToInteger = U.ifoldl' go 0
  where
    go :: Integer -> Int -> Bit -> Integer
    go acc idx (Bit b)
      | b = setBit acc idx
      | otherwise = acc

data JamState = JamState
  { _jamStateCache :: HashMap (Term Natural) Int,
    _jamStateBuilder :: Builder Bit
  }

initJamState :: JamState
initJamState =
  JamState
    { _jamStateCache = mempty,
      _jamStateBuilder = mempty
    }

data CueState = CueState
  { _cueStateBits :: Bit.Vector Bit,
    _cueStateBitPos :: Int,
    _cueStateStartPos :: Int,
    _cueStateCache :: HashMap Int (Term Natural)
  }

initCueState :: Bit.Vector Bit -> CueState
initCueState v =
  CueState
    { _cueStateBits = v,
      _cueStateBitPos = 0,
      _cueStateStartPos = 0,
      _cueStateCache = mempty
    }

makeLenses ''JamState
makeLenses ''CueState

data ReadError
  = ReadErrorEnd
  | ReadErrorInvalidTag
  | ReadErrorCacheMiss
  | ReadErrorInvalidLength
  | ReadErrorExpectedAtom
  | ReadErrorInvalidAtom
  deriving stock (Show)

consumeBit :: (Members '[Error ReadError, State CueState] r) => ReadError -> Sem r Bit
consumeBit e = do
  bits <- gets (^. cueStateBits)
  pos <- gets (^. cueStateBitPos)
  nextBit <- maybe (throw e) return (bits U.!? pos)
  modify' (over cueStateBitPos (+ 1))
  return nextBit

registerElementStart :: (Members '[State CueState] r) => Sem r ()
registerElementStart = do
  pos <- gets (^. cueStateBitPos)
  modify' (set cueStateStartPos pos)

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

writeBit :: (Member (State JamState) r) => Bit -> Sem r ()
writeBit b = modify appendByte
  where
    appendByte :: JamState -> JamState
    appendByte = over jamStateBuilder (<> Builder.singleton b)

writeOne :: (Member (State JamState) r) => Sem r ()
writeOne = writeBit (Bit True)

writeZero :: (Member (State JamState) r) => Sem r ()
writeZero = writeBit (Bit False)

writeIntegral :: (Integral a, Member (State JamState) r) => a -> Sem r ()
writeIntegral i = modify updateBuilder
  where
    iBuilder :: Builder Bit
    iBuilder = integerToBuilder i

    updateBuilder :: JamState -> JamState
    updateBuilder = over jamStateBuilder (<> iBuilder)

writeLength :: forall r. (Member (State JamState) r) => Int -> Sem r ()
writeLength len = do
  let lenOfLen = finiteBitSize len - countLeadingZeros len
  replicateM_ lenOfLen writeZero
  writeOne
  unless (lenOfLen == 0) (go len)
  where
    go :: Int -> Sem r ()
    -- Exclude the most significant bit of the length
    go l = unless (l == 1) $ do
      writeBit (Bit (testBit l 0))
      go (l `shiftR` 1)

writeAtomTag :: (Member (State JamState) r) => Sem r ()
writeAtomTag = writeZero

writeCellTag :: (Member (State JamState) r) => Sem r ()
writeCellTag = writeOne >> writeZero

writeBackrefTag :: (Member (State JamState) r) => Sem r ()
writeBackrefTag = writeOne >> writeOne

writeAtom :: forall r a. (Integral a, Member (State JamState) r) => Atom a -> Sem r ()
writeAtom a = do
  writeAtomTag
  writeLength (bitLength (a ^. atom))
  writeIntegral (a ^. atom)

cacheTerm :: (Member (State JamState) r) => Term Natural -> Sem r ()
cacheTerm t = do
  pos <- Builder.size <$> gets (^. jamStateBuilder)
  modify (set (jamStateCache . at t) (Just pos))

lookupCache :: (Member (State JamState) r) => Term Natural -> Sem r (Maybe Int)
lookupCache t = gets (^. jamStateCache . at t)

cacheCueTerm :: (Member (State CueState) r) => Term Natural -> Sem r ()
cacheCueTerm t = do
  pos <- gets (^. cueStateStartPos)
  modify' (set (cueStateCache . at pos) (Just t))

lookupCueCache :: (Members '[Error ReadError, State CueState] r) => Int -> Sem r (Term Natural)
lookupCueCache pos =
  fromMaybeM
    (throw ReadErrorCacheMiss)
    (gets (^. cueStateCache . at pos))

writeCell :: forall r. (Member (State JamState) r) => Cell Natural -> Sem r ()
writeCell c = do
  writeCellTag
  jamSem (c ^. cellLeft)
  jamSem (c ^. cellRight)

jamSem :: forall r. (Member (State JamState) r) => Term Natural -> Sem r ()
jamSem t = do
  ct <- lookupCache t
  case ct of
    Just idx -> case t of
      TermAtom a -> do
        let idxBitLength = finiteBitSize idx - countLeadingZeros idx
            atomBitLength = bitLength (a ^. atom)
        if
            | atomBitLength <= idxBitLength -> writeAtom a
            | otherwise -> backref idx
      TermCell {} -> backref idx
    Nothing -> do
      cacheTerm t
      case t of
        TermAtom a -> writeAtom a
        TermCell c -> writeCell c
  where
    backref :: Int -> Sem r ()
    backref idx = do
      writeBackrefTag
      writeLength (bitLength idx)
      writeIntegral idx

evalJamStateBuilder :: JamState -> Sem '[State JamState] a -> Builder Bit
evalJamStateBuilder st = (^. jamStateBuilder) . run . execState st

evalJamState :: JamState -> Sem '[State JamState] a -> Bit.Vector Bit
evalJamState st = build . evalJamStateBuilder st

jamToBuilder :: Term Natural -> Builder Bit
jamToBuilder = evalJamStateBuilder initJamState . jamSem

jamToVector :: Term Natural -> Bit.Vector Bit
jamToVector = build . jamToBuilder

jam :: Term Natural -> Atom Natural
jam = (\i -> Atom @Natural i emptyAtomInfo) . fromInteger . vectorBitsToInteger . jamToVector

cueToVector :: Atom Natural -> Bit.Vector Bit
cueToVector = integerToVectorBits . fromIntegral . (^. atom)

cueFromBits :: (Member (Error ReadError) r) => Bit.Vector Bit -> Sem r (Term Natural)
cueFromBits v = evalState (initCueState v) cueFromBitsSem

cueFromBitsSem :: forall r. (Members '[Error ReadError, State CueState] r) => Sem r (Term Natural)
cueFromBitsSem = do
  registerElementStart
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
    goBackref = undefined

    goCell :: Sem r (Term Natural)
    goCell = undefined

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
