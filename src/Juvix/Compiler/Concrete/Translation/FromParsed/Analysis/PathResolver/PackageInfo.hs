module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data PackageInfo = PackageInfo
  { _packageRoot :: Path Abs Dir,
    -- | files relative to the root of the package
    _packageRelativeFiles :: HashSet (Path Rel File),
    _packageAvailableRoots :: HashSet (Path Abs Dir),
    _packagePackage :: Package
  }
  deriving stock (Show)

makeLenses ''PackageInfo

packageFiles :: PackageInfo -> [Path Abs File]
packageFiles k = [k ^. packageRoot <//> f | f <- toList (k ^. packageRelativeFiles)]
