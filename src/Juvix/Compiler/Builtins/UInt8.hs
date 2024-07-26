module Juvix.Compiler.Builtins.UInt8 where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerUInt8 :: (Member Builtins r) => AxiomDef -> Sem r ()
registerUInt8 d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "UInt8 should be in the small universe")
  registerBuiltin BuiltinUInt8 (d ^. axiomName)

registerUInt8FromNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerUInt8FromNat d = do
  let l = getLoc d
  uint8 <- getBuiltinName l BuiltinUInt8
  nat <- getBuiltinName l BuiltinNat
  unless (d ^. axiomType === (nat --> uint8)) (error "uint8-from-nat has the wrong type signature")
  registerBuiltin BuiltinUInt8FromNat (d ^. axiomName)
