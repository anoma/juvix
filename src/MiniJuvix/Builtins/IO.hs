module MiniJuvix.Builtins.IO where

import MiniJuvix.Builtins.Effect
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra

registerIO :: Member Builtins r => AxiomDef -> Sem r ()
registerIO d = do
  unless (d ^. axiomType === smallUniverse) (error "IO should be in the small universe")
  registerBuiltin BuiltinIO (d ^. axiomName)

registerIOSequence :: Member Builtins r => AxiomDef -> Sem r ()
registerIOSequence d = do
  io <- getBuiltinName (getLoc d) BuiltinIO
  unless (d ^. axiomType === (io --> io --> io)) (error "IO sequence have type IO → IO")
  registerBuiltin BuiltinIOSequence (d ^. axiomName)
