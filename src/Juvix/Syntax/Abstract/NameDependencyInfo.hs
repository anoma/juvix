module Juvix.Syntax.Abstract.NameDependencyInfo(
    module Juvix.Syntax.Abstract.NameDependencyInfo,
    module Juvix.DependencyInfo
) where

import Juvix.DependencyInfo
import Juvix.Syntax.Abstract.Name

type NameDependencyInfo = DependencyInfo Name
