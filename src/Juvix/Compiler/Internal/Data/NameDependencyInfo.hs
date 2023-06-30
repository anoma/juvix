module Juvix.Compiler.Internal.Data.NameDependencyInfo
  ( module Juvix.Compiler.Internal.Data.NameDependencyInfo,
    module Juvix.Data.DependencyInfo,
  )
where

import Juvix.Compiler.Internal.Data.Name
import Juvix.Data.DependencyInfo

type NameDependencyInfo = DependencyInfo Name
