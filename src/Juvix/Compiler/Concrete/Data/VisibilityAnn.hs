module Juvix.Compiler.Concrete.Data.VisibilityAnn where

import Juvix.Extra.Serialize
import Juvix.Prelude

data VisibilityAnn
  = VisPublic
  | VisPrivate
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize VisibilityAnn

instance Semigroup VisibilityAnn where
  v1 <> v2 = case v1 of
    VisPublic -> VisPublic
    VisPrivate -> v2

instance Monoid VisibilityAnn where
  mempty = VisPrivate

resolveVisibility :: VisibilityAnn -> VisibilityAnn -> VisibilityAnn
resolveVisibility = (<>)
