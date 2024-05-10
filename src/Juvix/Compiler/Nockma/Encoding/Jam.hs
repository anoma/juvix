{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
An implmentation of the [Hoon jam](https://developers.urbit.org/reference/hoon/stdlib/2p#jam) function.

This is based on Urbit's [Rust implementation](https://github.com/urbit/noun/blob/4b22042623d7f3112b40c0f69138dc798e9bc56e/src/noun.rs#L175).
-}
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

-- | Write one bit to the output
writeBit :: forall a r. (Member (State (JamState a)) r) => Bit -> Sem r ()
writeBit b = modify appendBit
  where
    appendBit :: JamState a -> JamState a
    appendBit = over jamStateBuilder (<> Builder.singleton b)

-- | Write the bit 1 to the output
writeOne :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeOne = writeBit @a (Bit True)

-- | Write the bit 0 to the output
writeZero :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeZero = writeBit @a (Bit False)

-- | Write the binary encoding of the argument to the output
writeIntegral :: forall a b r. (Integral a, Member (State (JamState b)) r) => a -> Sem r ()
writeIntegral i = modify @(JamState b) updateBuilder
  where
    iBuilder :: Builder Bit
    iBuilder = integerToBuilder i

    updateBuilder :: JamState b -> JamState b
    updateBuilder = over jamStateBuilder (<> iBuilder)

-- | Write the binary encoding of argument interpreted as a length to the output
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

-- | Write the atom tag 0b0 to the output
writeAtomTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeAtomTag = writeZero @a

-- | Write the cell tag 0b01 to the output
writeCellTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeCellTag = writeOne @a >> writeZero @a

-- | Write the backref tag 0b11 to the output
writeBackrefTag :: forall a r. (Member (State (JamState a)) r) => Sem r ()
writeBackrefTag = writeOne @a >> writeOne @a

-- | Encode and write an atom to the output
writeAtom :: forall a r. (Integral a, Member (State (JamState a)) r) => Atom a -> Sem r ()
writeAtom a = do
  writeAtomTag @a
  writeLength @a (bitLength (a ^. atom))
  writeIntegral @a @a (a ^. atom)

-- | Encode and write a cell to the output
writeCell :: forall a r. (Hashable a, Integral a, Member (State (JamState a)) r) => Cell a -> Sem r ()
writeCell c = do
  writeCellTag @a
  jamSem @a (c ^. cellLeft)
  jamSem @a (c ^. cellRight)

-- | Encode and write a backref to the output
writeBackref :: forall a r. (Member (State (JamState a)) r) => Int -> Sem r ()
writeBackref idx = do
  writeBackrefTag @a
  writeLength @a (bitLength idx)
  writeIntegral @Int @a idx

-- | Cache the position of the encoding of the passed term
cacheTerm :: forall r a. (Hashable a, Member (State (JamState a)) r) => Term a -> Sem r ()
cacheTerm t = do
  pos <- Builder.size <$> gets @(JamState a) (^. jamStateBuilder)
  modify (set (jamStateCache . at t) (Just pos))

-- | Lookup the encoding of a term from the cache
lookupCache :: forall a r. (Hashable a, Member (State (JamState a)) r) => Term a -> Sem r (Maybe Int)
lookupCache t = gets @(JamState a) (^. jamStateCache . at t)

-- | Encode and write a Nock term to the output
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
            | otherwise -> writeBackref @a idx
      TermCell {} -> writeBackref @a idx
    Nothing -> do
      cacheTerm t
      case t of
        TermAtom a -> writeAtom a
        TermCell c -> writeCell c

evalJamStateBuilder :: JamState a -> Sem '[State (JamState a)] () -> Builder Bit
evalJamStateBuilder st = (^. jamStateBuilder) . run . execState st

evalJamState :: JamState a -> Sem '[State (JamState a)] () -> Bit.Vector Bit
evalJamState st = build . evalJamStateBuilder st

jamToBuilder :: forall a. (Integral a, Hashable a) => Term a -> Builder Bit
jamToBuilder = evalJamStateBuilder (initJamState @a) . jamSem

jamToVector :: (Integral a, Hashable a) => Term a -> Bit.Vector Bit
jamToVector = build . jamToBuilder

-- | jam encode a Nock term to an atom
jam :: forall a r. (Integral a, Hashable a, NockNatural a, Member (Error (ErrNockNatural a)) r) => Term a -> Sem r (Atom a)
jam t = do
  let i = fromInteger . vectorBitsToInteger . jamToVector $ t
  ai <- fromNatural i
  return (Atom ai emptyAtomInfo)
