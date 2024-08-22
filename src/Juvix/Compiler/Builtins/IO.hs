module Juvix.Compiler.Builtins.IO where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkIO :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkIO d =
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "IO should be in the small universe"

checkIOSequence :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkIOSequence d = do
  io <- getBuiltinNameScoper (getLoc d) BuiltinIO
  unless (d ^. axiomType === (io --> io --> io)) $
    builtinsErrorText (getLoc d) "IO sequence has type IO → IO"
