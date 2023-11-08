module Juvix.Compiler.Pipeline.Root.Base where

import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude

data Root = Root
  { _rootRootDir :: Path Abs Dir,
    _rootPackage :: Package,
    _rootPackageGlobal :: Bool,
    _rootBuildDir :: BuildDir,
    _rootInvokeDir :: Path Abs Dir
  }
  deriving stock (Show)

makeLenses ''Root
