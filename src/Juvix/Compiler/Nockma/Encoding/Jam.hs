module Juvix.Compiler.Nockma.Encoding.Jam where

import Data.Bit as Bit
import Data.Bits
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

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

makeLenses ''JamState

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

writeCell :: forall r. (Member (State JamState) r) => Cell Natural -> Sem r ()
writeCell c = do
  writeCellTag
  jamSem (c ^. cellLeft)
  jamSem (c ^. cellRight)

cacheTerm :: (Member (State JamState) r) => Term Natural -> Sem r ()
cacheTerm t = do
  pos <- Builder.size <$> gets (^. jamStateBuilder)
  modify (set (jamStateCache . at t) (Just pos))

lookupCache :: (Member (State JamState) r) => Term Natural -> Sem r (Maybe Int)
lookupCache t = gets (^. jamStateCache . at t)

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
