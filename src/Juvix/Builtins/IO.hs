module Juvix.Builtins.IO where

import Juvix.Builtins.Effect
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language.Extra

registerIO :: Member Builtins r => AxiomDef -> Sem r ()
registerIO d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "IO should be in the small universe")
  registerBuiltin BuiltinIO (d ^. axiomName)

registerIOSequence :: Member Builtins r => AxiomDef -> Sem r ()
registerIOSequence d = do
  io <- getBuiltinName (getLoc d) BuiltinIO
  unless (d ^. axiomType === (io --> io --> io)) (error "IO sequence have type IO → IO")
  registerBuiltin BuiltinIOSequence (d ^. axiomName)
