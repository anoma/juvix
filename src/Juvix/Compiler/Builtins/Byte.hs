module Juvix.Compiler.Builtins.Byte where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerByte :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByte d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "UInt8 should be in the small universe")
  registerBuiltin BuiltinByte (d ^. axiomName)

registerByteEq :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteEq f = do
  uint8 <- getBuiltinName (getLoc f) BuiltinByte
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (uint8 --> uint8 --> bool_)) (error "UInt8 equality has the wrong type signature")
  registerBuiltin BuiltinByteEq (f ^. axiomName)

registerByteFromNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteFromNat d = do
  let l = getLoc d
  uint8 <- getBuiltinName l BuiltinByte
  nat <- getBuiltinName l BuiltinNat
  unless (d ^. axiomType === (nat --> uint8)) (error "uint8-from-nat has the wrong type signature")
  registerBuiltin BuiltinByteFromNat (d ^. axiomName)

registerByteToNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteToNat f = do
  uint8 <- getBuiltinName (getLoc f) BuiltinByte
  nat_ <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (uint8 --> nat_)) (error "uint8-to-nat conversion has the wrong type signature")
  registerBuiltin BuiltinByteToNat (f ^. axiomName)
