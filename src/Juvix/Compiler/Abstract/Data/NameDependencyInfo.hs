module Juvix.Compiler.Abstract.Data.NameDependencyInfo
  ( module Juvix.Compiler.Abstract.Data.NameDependencyInfo,
    module Juvix.Data.DependencyInfo,
  )
where

import Juvix.Compiler.Abstract.Data.Name
import Juvix.Data.DependencyInfo

type NameDependencyInfo = DependencyInfo Name
