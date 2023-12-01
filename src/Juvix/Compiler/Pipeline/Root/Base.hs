module Juvix.Compiler.Pipeline.Root.Base where

import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude

data PackageType
  = GlobalStdlib
  | GlobalPackageBase
  | GlobalPackageDescription
  | LocalPackage
  deriving stock (Eq, Show)

data Root = Root
  { _rootRootDir :: Path Abs Dir,
    _rootPackage :: Package,
    _rootPackageType :: PackageType,
    _rootBuildDir :: BuildDir,
    _rootInvokeDir :: Path Abs Dir
  }
  deriving stock (Show)

makeLenses ''Root
