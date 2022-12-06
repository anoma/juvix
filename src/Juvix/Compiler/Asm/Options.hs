module Juvix.Compiler.Asm.Options
  ( module Juvix.Compiler.Asm.Options,
    module Juvix.Compiler.Backend,
  )
where

import Juvix.Compiler.Backend
import Juvix.Prelude

data Options = Options
  { _optDebug :: Bool,
    _optLimits :: Limits
  }

makeLenses ''Options

makeOptions :: Target -> Bool -> Options
makeOptions tgt debug =
  Options
    { _optDebug = debug,
      _optLimits = getLimits tgt debug
    }

getClosureSize :: Options -> Int -> Int
getClosureSize opts argsNum = opts ^. optLimits . limitsClosureHeadSize + argsNum
