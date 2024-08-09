module Juvix.Compiler.Builtins.ByteArray where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerByteArray :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteArray d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "ByteArray should be in the small universe")
  registerBuiltin BuiltinByteArray (d ^. axiomName)

registerByteArrayFromListByte :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteArrayFromListByte d = do
  let loc = getLoc d
  byte_ <- getBuiltinName loc BuiltinByte
  list_ <- getBuiltinName loc BuiltinList
  byteArray <- getBuiltinName loc BuiltinByteArray
  unless (d ^. axiomType == (list_ @@ byte_ --> byteArray)) (error "bytearray-from-list-byte has the wrong type")
  registerBuiltin BuiltinByteArrayFromListByte (d ^. axiomName)

registerByteArrayLength :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteArrayLength d = do
  let loc = getLoc d
  byteArray <- getBuiltinName loc BuiltinByteArray
  nat_ <- getBuiltinName loc BuiltinNat
  unless (d ^. axiomType == (byteArray --> nat_)) (error "bytearray-length has the wrong type")
  registerBuiltin BuiltinByteArrayLength (d ^. axiomName)
