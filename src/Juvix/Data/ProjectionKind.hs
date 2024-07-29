module Juvix.Data.ProjectionKind where

import Juvix.Prelude.Base

data ProjectionKind
  = -- | Projection for regular fields
    ProjectionExplicit
  | -- | Projection for instance fields
    ProjectionCoercion
  deriving stock (Show, Eq, Ord, Generic)
