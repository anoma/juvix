module Juvix.Compiler.Concrete.Data.IfBranchKind where

import Juvix.Extra.Serialize
import Juvix.Prelude

data IfBranchKind
  = -- | Boolean condition
    BranchIfBool
  | -- | Default branch
    BranchIfElse
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize IfBranchKind

instance NFData IfBranchKind

$(genSingletons [''IfBranchKind])
