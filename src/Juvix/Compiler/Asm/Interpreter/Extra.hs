module Juvix.Compiler.Asm.Interpreter.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Prelude

frameFromFunctionInfo :: FunctionInfo -> [Val] -> Frame
frameFromFunctionInfo fi args =
  Frame
    { _frameArgs =
        ArgumentArea
          { _argumentArea = HashMap.fromList (zip [0 ..] args),
            _argumentAreaSize = fi ^. functionInfoArgsNum
          },
      _frameTemp =
        TemporaryArea
          { _temporaryArea = mempty,
            _temporaryAreaSize = fi ^. functionInfoTempSize
          },
      _frameStack = ValueStack []
    }
