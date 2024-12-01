module Juvix.Compiler.Pipeline.Loader.PathResolver.GlobalVersions where

import Data.Versions
import Juvix.Prelude

newtype GlobalVersions = GlobalVersions
  { -- | This should be filled in iff one of the dependencies is the global standard library
    _globalVersionsStdlib :: Maybe SemVer
  }

makeLenses ''GlobalVersions
