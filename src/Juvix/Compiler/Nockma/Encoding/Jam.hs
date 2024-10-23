{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- An implmentation of the [Hoon jam](https://developers.urbit.org/reference/hoon/stdlib/2p#jam) function.
--
-- This is based on Urbit's [Rust implementation](https://github.com/urbit/noun/blob/4b22042623d7f3112b40c0f69138dc798e9bc56e/src/noun.rs#L175).
module Juvix.Compiler.Nockma.Encoding.Jam where

import Data.Bit as Bit
import Data.Bits
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Encoding.ByteString
import Juvix.Compiler.Nockma.Encoding.Effect.BitWriter
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

newtype JamState a = JamState
  { _jamStateCache :: HashMap (Term a) Int
  }

initJamState :: forall a. (Hashable a) => JamState a
initJamState =
  JamState
    { _jamStateCache = mempty
    }

makeLenses ''JamState

-- | Write the atom tag 0b0 to the output
writeAtomTag :: (Member BitWriter r) => Sem r ()
writeAtomTag = writeZero

-- | Write the cell tag 0b10 to the output
writeCellTag :: (Member BitWriter r) => Sem r ()
writeCellTag = writeOne >> writeZero

-- | Write the backref tag 0b11 to the output
writeBackrefTag :: (Member BitWriter r) => Sem r ()
writeBackrefTag = writeOne >> writeOne

-- | Encode and write an atom to the output
writeAtom :: (Integral a, Member BitWriter r) => Atom a -> Sem r ()
writeAtom a = do
  writeAtomTag
  writeLength (bitLength (a ^. atom))
  writeIntegral (a ^. atom)

-- | Encode and write a cell to the output
writeCell :: (Hashable a, Integral a, Members '[BitWriter, State (JamState a)] r) => Cell a -> Sem r ()
writeCell c = do
  writeCellTag
  jamSem (c ^. cellLeft)
  jamSem (c ^. cellRight)

-- | Encode and write a backref to the output
writeBackref :: (Member BitWriter r) => Int -> Sem r ()
writeBackref idx = do
  writeBackrefTag
  writeLength (bitLength idx)
  writeIntegral idx

-- | Cache the position of the encoding of the passed term
cacheTerm :: forall r a. (Hashable a, Members '[BitWriter, State (JamState a)] r) => Term a -> Sem r ()
cacheTerm t = do
  pos <- getCurrentPosition
  modify (set (jamStateCache . at t) (Just pos))

-- | Lookup the encoding of a term from the cache
lookupCache :: forall a r. (Hashable a, Member (State (JamState a)) r) => Term a -> Sem r (Maybe Int)
lookupCache t = gets @(JamState a) (^. jamStateCache . at t)

-- | Encode and write a Nock term to the output
jamSem :: forall a r. (Integral a, Hashable a, Members '[BitWriter, State (JamState a)] r) => Term a -> Sem r ()
jamSem t = do
  ct <- lookupCache @a t
  case ct of
    Just idx -> case t of
      TermAtom a -> do
        let idxBitLength = finiteBitSize idx - countLeadingZeros idx
            atomBitLength = bitLength (a ^. atom)
        if
            | atomBitLength <= idxBitLength -> writeAtom a
            | otherwise -> writeBackref idx
      TermCell {} -> writeBackref idx
    Nothing -> do
      cacheTerm t
      case t of
        TermAtom a -> writeAtom a
        TermCell c -> writeCell c

jamToBits :: forall a. (Integral a, Hashable a) => Term a -> Bit.Vector Bit
jamToBits =
  run
    . execBitWriter
    . evalState (initJamState @a)
    . jamSem

-- | jam encode a Nock term to the bytes encoding of an atom
jamToByteString :: forall a. (Integral a, Hashable a) => Term a -> ByteString
jamToByteString = vectorBitsToByteString . jamToBits

-- | jam encode a Nock term to an atom
jam :: forall a r. (Integral a, Hashable a, NockNatural a, Member (Error (ErrNockNatural a)) r) => Term a -> Sem r (Atom a)
jam t = do
  let i = fromInteger . vectorBitsToInteger . jamToBits $ t
  ai <- fromNatural i
  return (Atom ai emptyAtomInfo)
