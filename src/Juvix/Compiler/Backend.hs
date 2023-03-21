module Juvix.Compiler.Backend where

import GHC.Base (maxInt)
import Juvix.Prelude

data Target = TargetCWasm32Wasi | TargetCNative64 | TargetGeb | TargetCore | TargetAsm
  deriving stock (Data, Eq, Show)

data Limits = Limits
  { _limitsMaxConstrs :: Int,
    _limitsMaxConstrArgs :: Int,
    _limitsMaxFunctionArgs :: Int,
    _limitsMaxLocalVars :: Int,
    _limitsMaxClosureSize :: Int,
    _limitsClosureHeadSize :: Int,
    _limitsMaxStringSize :: Int,
    _limitsMaxStackDelta :: Int,
    _limitsMaxFunctionAlloc :: Int,
    _limitsDispatchStackSize :: Int,
    _limitsBuiltinUIDsNum :: Int
  }
  deriving stock (Eq, Show)

makeLenses ''Limits

-- Make sure the following limits correspond to the limits in the runtime for
-- each backend target
getLimits :: Target -> Bool -> Limits
getLimits tgt debug = case tgt of
  TargetCWasm32Wasi ->
    Limits
      { _limitsMaxConstrs = 1048568,
        _limitsMaxConstrArgs = 255,
        _limitsMaxFunctionArgs = 253,
        _limitsMaxLocalVars = 2048,
        _limitsMaxClosureSize = 253 + 3,
        _limitsClosureHeadSize = if debug then 3 else 2,
        _limitsMaxStringSize = 255 + 1,
        _limitsMaxStackDelta = 16368,
        _limitsMaxFunctionAlloc = 16368,
        _limitsDispatchStackSize = 4,
        _limitsBuiltinUIDsNum = 8
      }
  TargetCNative64 ->
    Limits
      { _limitsMaxConstrs = 1048568,
        _limitsMaxConstrArgs = 255,
        _limitsMaxFunctionArgs = 253,
        _limitsMaxLocalVars = 1024,
        _limitsMaxClosureSize = 253 + 3,
        _limitsClosureHeadSize = if debug then 3 else 2,
        _limitsMaxStringSize = 255 + 1,
        _limitsMaxStackDelta = 8184,
        _limitsMaxFunctionAlloc = 8184,
        _limitsDispatchStackSize = 4,
        _limitsBuiltinUIDsNum = 8
      }
  TargetGeb ->
    defaultLimits
  TargetCore ->
    defaultLimits
  TargetAsm ->
    defaultLimits

defaultLimits :: Limits
defaultLimits =
  Limits
    { _limitsMaxConstrs = maxInt,
      _limitsMaxConstrArgs = maxInt,
      _limitsMaxFunctionArgs = maxInt,
      _limitsMaxLocalVars = maxInt,
      _limitsMaxClosureSize = maxInt,
      _limitsClosureHeadSize = maxInt,
      _limitsMaxStackDelta = maxInt,
      _limitsMaxFunctionAlloc = maxInt,
      _limitsDispatchStackSize = maxInt,
      _limitsBuiltinUIDsNum = maxInt
    }
