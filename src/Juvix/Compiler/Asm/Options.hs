module Juvix.Compiler.Asm.Options
  ( module Juvix.Compiler.Asm.Options,
    module Juvix.Compiler.Backend,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Options qualified as Tree
import Juvix.Prelude

data Options = Options
  { _optDebug :: Bool,
    _optLimits :: Limits,
    _optTreeOptions :: Tree.Options
  }

makeLenses ''Options

makeOptions :: Target -> Bool -> Options
makeOptions tgt debug =
  Options
    { _optDebug = debug,
      _optLimits = getLimits tgt debug,
      _optTreeOptions = Tree.defaultOptions
    }

getClosureSize :: Options -> Int -> Int
getClosureSize opts argsNum = opts ^. optLimits . limitsClosureHeadSize + argsNum

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint e@EntryPoint {..} =
  Options
    { _optDebug = _entryPointDebug,
      _optLimits = getLimits (getEntryPointTarget e) _entryPointDebug,
      _optTreeOptions = Tree.fromEntryPoint e
    }
