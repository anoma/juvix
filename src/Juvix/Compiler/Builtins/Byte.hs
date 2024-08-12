module Juvix.Compiler.Builtins.Byte where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkByte :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkByte d = do
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "Byte should be in the small universe"

checkByteEq :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteEq f = do
  byte_ <- getBuiltinNameScoper (getLoc f) BuiltinByte
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  unless (f ^. axiomType === (byte_ --> byte_ --> bool_)) $
    builtinsErrorText (getLoc f) "Byte equality has the wrong type signature"

checkByteFromNat :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteFromNat d = do
  let l = getLoc d
  byte_ <- getBuiltinNameScoper l BuiltinByte
  nat <- getBuiltinNameScoper l BuiltinNat
  unless (d ^. axiomType === (nat --> byte_)) $
    builtinsErrorText (getLoc d) "byte-from-nat has the wrong type signature"

checkByteToNat :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteToNat f = do
  byte_ <- getBuiltinNameScoper (getLoc f) BuiltinByte
  nat_ <- getBuiltinNameScoper (getLoc f) BuiltinNat
  unless (f ^. axiomType === (byte_ --> nat_)) $
    builtinsErrorText (getLoc f) "byte-to-nat has the wrong type signature"
