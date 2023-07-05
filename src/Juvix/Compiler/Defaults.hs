module Juvix.Compiler.Defaults where

import Juvix.Prelude

defaultUnrollLimit :: Int
defaultUnrollLimit = 140

defaultOptimizationLevel :: Int
defaultOptimizationLevel = 1

defaultInliningDepth :: Int
defaultInliningDepth = 2

defaultStackSize :: Int
defaultStackSize = 1024

defaultHeapSize :: Int
defaultHeapSize = 1024

defaultVampIRIntegerBits :: Int
defaultVampIRIntegerBits = 24
