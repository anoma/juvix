module Juvix.Compiler.Casm.Extra.Stdlib where

import Data.FileEmbed qualified as FE
import Data.Text.Encoding
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromSource

data StdlibBuiltins = StdlibBuiltins
  { _stdlibGetRegs :: Symbol,
    _stdlibCallClosure :: Symbol,
    _stdlibExtendClosure :: Symbol
  }

makeLenses ''StdlibBuiltins

addStdlibBuiltins :: (Member LabelInfoBuilder r) => Sem r (StdlibBuiltins, [Instruction])
addStdlibBuiltins = do
  instrs <-
    fmap (fromRight impossible) $
      runParser' "stdlib.casm" $
        decodeUtf8 $(FE.makeRelativeToProject "runtime/src/casm/stdlib.casm" >>= FE.embedFile)
  _stdlibGetRegs <- fromJust <$> getIdent "juvix_get_regs"
  _stdlibCallClosure <- fromJust <$> getIdent "juvix_call_closure"
  _stdlibExtendClosure <- fromJust <$> getIdent "juvix_extend_closure"
  return (StdlibBuiltins {..}, instrs)
