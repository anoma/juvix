module Juvix.Compiler.Pipeline.Root.Base where

import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude

data Roots = Roots
  { _rootsRootDir :: Path Abs Dir,
    _rootsPackage :: Package,
    _rootsPackageGlobal :: Bool,
    _rootsBuildDir :: Path Abs Dir,
    _rootsInvokeDir :: Path Abs Dir
  }
  deriving stock (Show)

makeLenses ''Roots
