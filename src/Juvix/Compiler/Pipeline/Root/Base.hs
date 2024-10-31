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
  { _rootSomeRoot :: SomeRoot,
    _rootBuildDir :: BuildDir,
    _rootInvokeDir :: Path Abs Dir
  }

-- | TODO think of a better name
data SomeRoot = SomeRoot
  { _someRootDir :: Path Abs Dir,
    _someRootType :: PackageType
  }
  deriving stock (Eq, Show)

makeLenses ''Root
makeLenses ''SomeRoot

rootRootDir :: Lens' Root (Path Abs Dir)
rootRootDir = undefined

rootPackageType :: Lens' Root PackageType
rootPackageType = undefined
