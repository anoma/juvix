module Juvix.Compiler.Backend where

import GHC.Base (maxInt)
import Juvix.Prelude

data Target
  = TargetCWasm32Wasi
  | TargetCNative64
  | TargetGeb
  | TargetVampIR
  | TargetCore
  | TargetAsm
  | TargetReg
  | TargetTree
  | TargetNockma
  | TargetAnoma
  | TargetCairo
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
    _limitsBuiltinUIDsNum :: Int,
    _limitsSpecialisedApply :: Int
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
        _limitsBuiltinUIDsNum = 8,
        _limitsSpecialisedApply = 3
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
        _limitsBuiltinUIDsNum = 8,
        _limitsSpecialisedApply = 3
      }
  TargetGeb ->
    defaultLimits
  TargetVampIR ->
    defaultLimits
  TargetCore ->
    defaultLimits
  TargetAsm ->
    defaultLimits
  TargetReg ->
    Limits
      { _limitsMaxConstrs = 1048568,
        _limitsMaxConstrArgs = 255,
        _limitsMaxFunctionArgs = 253,
        _limitsMaxLocalVars = 2048,
        _limitsMaxClosureSize = 253 + 3,
        _limitsClosureHeadSize = 2,
        _limitsMaxStringSize = 255 + 1,
        _limitsMaxStackDelta = 16368,
        _limitsMaxFunctionAlloc = 16368,
        _limitsDispatchStackSize = 4,
        _limitsBuiltinUIDsNum = 8,
        _limitsSpecialisedApply = 3
      }
  TargetTree ->
    defaultLimits
  TargetNockma ->
    defaultLimits
  TargetAnoma ->
    defaultLimits
  TargetCairo ->
    Limits
      { _limitsMaxConstrs = 1048568,
        _limitsMaxConstrArgs = 255,
        _limitsMaxFunctionArgs = 8,
        _limitsMaxLocalVars = 2048,
        _limitsMaxClosureSize = 8 + 3,
        _limitsClosureHeadSize = 3,
        _limitsMaxStringSize = 0,
        _limitsMaxStackDelta = 0, -- irrelevant
        _limitsMaxFunctionAlloc = 0, -- irrelevant
        _limitsDispatchStackSize = 0, -- irrelevant
        _limitsBuiltinUIDsNum = 8,
        _limitsSpecialisedApply = 3
      }

defaultLimits :: Limits
defaultLimits =
  Limits
    { _limitsMaxConstrs = maxInt,
      _limitsMaxConstrArgs = maxInt,
      _limitsMaxFunctionArgs = maxInt,
      _limitsMaxLocalVars = maxInt,
      _limitsMaxClosureSize = maxInt,
      _limitsClosureHeadSize = maxInt,
      _limitsMaxStringSize = maxInt,
      _limitsMaxStackDelta = maxInt,
      _limitsMaxFunctionAlloc = maxInt,
      _limitsDispatchStackSize = maxInt,
      _limitsBuiltinUIDsNum = maxInt,
      _limitsSpecialisedApply = 0
    }
