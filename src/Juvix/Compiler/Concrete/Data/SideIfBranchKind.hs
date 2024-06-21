module Juvix.Compiler.Concrete.Data.SideIfBranchKind where

import Juvix.Extra.Serialize
import Juvix.Prelude

data SideIfBranchKind
  = -- | Boolean condition
    SideIfBool
  | -- | Default branch
    SideIfElse
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize SideIfBranchKind

instance NFData SideIfBranchKind

$(genSingletons [''SideIfBranchKind])
