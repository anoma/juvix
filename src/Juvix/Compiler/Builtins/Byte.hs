module Juvix.Compiler.Builtins.Byte where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerByte :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByte d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "Byte should be in the small universe")
  registerBuiltin BuiltinByte (d ^. axiomName)

registerByteEq :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteEq f = do
  byte_ <- getBuiltinName (getLoc f) BuiltinByte
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (byte_ --> byte_ --> bool_)) (error "Byte equality has the wrong type signature")
  registerBuiltin BuiltinByteEq (f ^. axiomName)

registerByteFromNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteFromNat d = do
  let l = getLoc d
  byte_ <- getBuiltinName l BuiltinByte
  nat <- getBuiltinName l BuiltinNat
  unless (d ^. axiomType === (nat --> byte_)) (error "byte-from-nat has the wrong type signature")
  registerBuiltin BuiltinByteFromNat (d ^. axiomName)

registerByteToNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteToNat f = do
  byte_ <- getBuiltinName (getLoc f) BuiltinByte
  nat_ <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (byte_ --> nat_)) (error "byte-to-nat has the wrong type signature")
  registerBuiltin BuiltinByteToNat (f ^. axiomName)
