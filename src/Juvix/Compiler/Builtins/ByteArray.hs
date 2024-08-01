module Juvix.Compiler.Builtins.ByteArray where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerByteArray :: (Member Builtins r) => AxiomDef -> Sem r ()
registerByteArray d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "ByteArray should be in the small universe")
  registerBuiltin BuiltinByte (d ^. axiomName)
