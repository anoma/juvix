module Juvix.Compiler.Builtins.String where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerString :: Member Builtins r => AxiomDef -> Sem r ()
registerString d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "String should be in the small universe")
  registerBuiltin BuiltinString (d ^. axiomName)

registerStringPrint :: Member Builtins r => AxiomDef -> Sem r ()
registerStringPrint f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (string_ --> io)) (error "String print has the wrong type signature")
  registerBuiltin BuiltinStringPrint (f ^. axiomName)
