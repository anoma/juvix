{-# LANGUAGE AllowAmbiguousTypes #-}

module Juvix.Compiler.Nockma.Encoding.Jam where

import Data.Bit as Bit
import Data.Bits
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

data JamState a = JamState
  { _jamStateCache :: HashMap (Term a) Int,
    _jamStateBuilder :: Builder Bit
  }

initJamState :: forall a. (Hashable a) => JamState a
initJamState =
  JamState
    { _jamStateCache = mempty,
      _jamStateBuilder = mempty
    }

makeLenses ''JamState

writeBit :: forall a r. (Member (State (JamState a)) r) => Bit -> Sem r ()
writeBit b = modify appendByte
  where
    appendByte :: JamState a -> JamState a
    appendByte = over jamStateBuilder (<> Builder.singleton b)

writeOne :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeOne = writeBit @a (Bit True)

writeZero :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeZero = writeBit @a (Bit False)

writeIntegral :: forall a b r. (Integral a, Member (State (JamState b)) r) => a -> Sem r ()
writeIntegral i = modify @(JamState b) updateBuilder
  where
    iBuilder :: Builder Bit
    iBuilder = integerToBuilder i

    updateBuilder :: JamState b -> JamState b
    updateBuilder = over jamStateBuilder (<> iBuilder)

writeLength :: forall a r. (Member (State (JamState a)) r) => Int -> Sem r ()
writeLength len = do
  let lenOfLen = finiteBitSize len - countLeadingZeros len
  replicateM_ lenOfLen (writeZero @a)
  writeOne @a
  unless (lenOfLen == 0) (go len)
  where
    go :: Int -> Sem r ()
    -- Exclude the most significant bit of the length
    go l = unless (l == 1) $ do
      writeBit @a (Bit (testBit l 0))
      go (l `shiftR` 1)

writeAtomTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeAtomTag = writeZero @a

writeCellTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeCellTag = writeOne @a >> writeZero @a

writeBackrefTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeBackrefTag = writeOne @a >> writeOne @a

writeAtom :: forall a r. (Integral a, Member (State (JamState a)) r) => Atom a -> Sem r ()
writeAtom a = do
  writeAtomTag @a
  writeLength @a (bitLength (a ^. atom))
  writeIntegral @a @a (a ^. atom)

writeCell :: forall a r. (Hashable a, Integral a, Member (State (JamState a)) r) => Cell a -> Sem r ()
writeCell c = do
  writeCellTag @a
  jamSem @a (c ^. cellLeft)
  jamSem @a (c ^. cellRight)

cacheTerm :: forall r a. (Hashable a, Member (State (JamState a)) r) => Term a -> Sem r ()
cacheTerm t = do
  pos <- Builder.size <$> gets @(JamState a) (^. jamStateBuilder)
  modify (set (jamStateCache . at t) (Just pos))

lookupCache :: forall a r. (Hashable a, Member (State (JamState a)) r) => Term a -> Sem r (Maybe Int)
lookupCache t = gets @(JamState a) (^. jamStateCache . at t)

jamSem :: forall a r. (Integral a, Hashable a, Member (State (JamState a)) r) => Term a -> Sem r ()
jamSem t = do
  ct <- lookupCache @a t
  case ct of
    Just idx -> case t of
      TermAtom a -> do
        let idxBitLength = finiteBitSize idx - countLeadingZeros idx
            atomBitLength = bitLength (a ^. atom)
        if
            | atomBitLength <= idxBitLength -> writeAtom @a a
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
      writeBackrefTag @a
      writeLength @a (bitLength idx)
      writeIntegral @Int @a idx

evalJamStateBuilder :: JamState a -> Sem '[State (JamState a)] () -> Builder Bit
evalJamStateBuilder st = (^. jamStateBuilder) . run . execState st

evalJamState :: JamState a -> Sem '[State (JamState a)] () -> Bit.Vector Bit
evalJamState st = build . evalJamStateBuilder st

jamToBuilder :: forall a. (Integral a, Hashable a) => Term a -> Builder Bit
jamToBuilder = evalJamStateBuilder (initJamState @a) . jamSem

jamToVector :: (Integral a, Hashable a) => Term a -> Bit.Vector Bit
jamToVector = build . jamToBuilder

jam :: forall a r. (Integral a, Hashable a, NockNatural a, Member (Error (ErrNockNatural a)) r) => Term a -> Sem r (Atom a)
jam t = do
  let i = fromInteger . vectorBitsToInteger . jamToVector $ t
  ai <- fromNatural i
  return (Atom ai emptyAtomInfo)
