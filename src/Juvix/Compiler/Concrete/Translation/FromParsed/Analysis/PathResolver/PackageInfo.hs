module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data PackageInfo = PackageInfo
  { _packageRoot :: FilePath,
    _packageRelativeFiles :: HashSet FilePath,
    _packageAvailableRoots :: HashSet FilePath,
    _packagePackage :: Package
  }
  deriving stock (Show)

makeLenses ''PackageInfo
