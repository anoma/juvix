{-# LANGUAGE TemplateHaskell #-}

module Mari.Library.Trace.Types where

import qualified Control.Lens as Lens hiding ((|>))
import qualified Data.Text as T
import Mari.Library
import qualified Mari.Library.HashMap as HashMap
import qualified Mari.Library.NameSymbol as NameSymbol

data T = T
  { -- | @tCurrent@ represents the current stack-trace we are computing
    -- under, We use a StackChain to determine the trace the current
    -- trace is computing under.
    tCurrent :: StackChain,
    -- | @tTraces@ represents our current trace log of all completed
    -- functions
    tTraces :: Log,
    -- | @tEnabled@ represents meta information our system has regards
    -- to if particular trace functions are enabled and if there is a
    -- general debug level we can print out instead.
    tEnabled :: HashMap.T NameSymbol.T MetaInfo,
    -- | @tDebugLevel@ represents the debug level we care about
    tDebugLevel :: Maybe Natural
  }
  deriving (Show, Eq)

type Log = [Stack]

data StackChain
  = Empty
  | StackChain
      { parent :: StackChain,
        currentStack :: Stack
      }
  deriving (Show, Eq)

-- | @Stack@ represents a stack-trace for a particular function
data Stack = Stack
  { -- | @stackName@ represents the name of the stackd function
    stackName :: NameSymbol.T,
    -- | @stackdStart@ represents the incoming function arguments
    stackStart :: [T.Text],
    -- | @stackBetween@ represents the Traces that have happened
    -- between this call and the end of the call
    stackBetween :: Log,
    -- | @stackOutput@ represents the output result. Maybe as we could
    -- stop before it's finished!
    stackOutput :: Maybe T.Text
  }
  deriving (Show, Eq)

data MetaInfo = MetaInfo
  { metaInfoEnable :: Enable,
    metaInfoLevel :: Natural
  }
  deriving (Show, Eq)

data Enable
  = -- | @Enabled@ represents a trace enabled function
    Enabled
  | -- | @Disabled@ represents that the function is disabled
    Disabled
  | -- | @DisableRecursive@ represents that we shouldn't trace any
    -- Enabled functions inside the scope of the Trace
    DisableRecursive
  deriving (Show, Eq)

newtype Error = Error T

Lens.makeLensesWith Lens.camelCaseFields ''Stack
Lens.makeLensesWith Lens.camelCaseFields ''T
Lens.makeLensesWith Lens.camelCaseFields ''MetaInfo

type Eff m = HasState "trace" T m

maxTrace :: Natural
maxTrace = 10

defaultTrace :: Natural
defaultTrace = 3
