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
    _stdlibPoseidon :: Symbol,
    _stdlibEcOp :: Symbol,
    _stdlibGetRegsName :: Text,
    _stdlibCallClosureName :: Text,
    _stdlibExtendClosureName :: Text,
    _stdlibPoseidonName :: Text,
    _stdlibEcOpName :: Text,
    -- The Ap offsets include the 2 cells for function call
    _stdlibGetRegsApOffset :: Int,
    _stdlibPoseidonApOffset :: Int,
    _stdlibEcOpApOffset :: Int
  }

makeLenses ''StdlibBuiltins

addStdlibBuiltins :: (Member LabelInfoBuilder r) => Address -> Sem r (StdlibBuiltins, [Instruction])
addStdlibBuiltins addr = do
  instrs <-
    fmap (fromRight impossible)
      $ runParser' addr "stdlib.casm"
      $ decodeUtf8 $(FE.makeRelativeToProject "runtime/casm/stdlib.casm" >>= FE.embedFile)
  let _stdlibGetRegsName :: Text = "juvix_get_regs"
      _stdlibCallClosureName :: Text = "juvix_call_closure"
      _stdlibExtendClosureName :: Text = "juvix_extend_closure"
      _stdlibPoseidonName :: Text = "juvix_poseidon"
      _stdlibEcOpName :: Text = "juvix_ec_op"
      -- Make sure the Ap offsets correspond to the runtime
      _stdlibGetRegsApOffset :: Int = 4
      _stdlibPoseidonApOffset :: Int = 18
      _stdlibEcOpApOffset :: Int = 18
  _stdlibGetRegs <- fromJust <$> getIdent _stdlibGetRegsName
  _stdlibCallClosure <- fromJust <$> getIdent _stdlibCallClosureName
  _stdlibExtendClosure <- fromJust <$> getIdent _stdlibExtendClosureName
  _stdlibPoseidon <- fromJust <$> getIdent _stdlibPoseidonName
  _stdlibEcOp <- fromJust <$> getIdent _stdlibEcOpName
  return (StdlibBuiltins {..}, instrs)
