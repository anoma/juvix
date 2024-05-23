module Juvix.Compiler.Nockma.Encoding.ByteString where

import Data.Bit
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

atomToByteString :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
atomToByteString am = do
  n <- nockNatural am
  return (cloneToByteString . integerToVectorBits . toInteger $ n)

byteStringToAtom :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => ByteString -> Sem r (Atom a)
byteStringToAtom bs = do
  a <- fromNatural . fromInteger . vectorBitsToInteger . cloneFromByteString $ bs
  return
    Atom
      { _atomInfo = emptyAtomInfo,
        _atom = a
      }
