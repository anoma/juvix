module Juvix.Compiler.Casm.Extra.Stdlib where

import Data.FileEmbed qualified as FE
import Data.Text.Encoding
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromSource

data StdlibBuiltins = StdlibBuiltins
  { _stdlibGetRegs :: Symbol,
    _stdlibCallClosure :: Symbol,
    _stdlibExtendClosure :: Symbol,
    _stdlibGetRegsName :: Text,
    _stdlibCallClosureName :: Text,
    _stdlibExtendClosureName :: Text
  }

makeLenses ''StdlibBuiltins

addStdlibBuiltins :: (Member LabelInfoBuilder r) => Address -> Sem r (StdlibBuiltins, [Instruction])
addStdlibBuiltins addr = do
  instrs <-
    fmap (fromRight impossible) $
      runParser' addr "stdlib.casm" $
        decodeUtf8 $(FE.makeRelativeToProject "runtime/src/casm/stdlib.casm" >>= FE.embedFile)
  let _stdlibGetRegsName :: Text = "juvix_get_regs"
      _stdlibCallClosureName :: Text = "juvix_call_closure"
      _stdlibExtendClosureName :: Text = "juvix_extend_closure"
  _stdlibGetRegs <- fromJust <$> getIdent _stdlibGetRegsName
  _stdlibCallClosure <- fromJust <$> getIdent _stdlibCallClosureName
  _stdlibExtendClosure <- fromJust <$> getIdent _stdlibExtendClosureName
  return (StdlibBuiltins {..}, instrs)
