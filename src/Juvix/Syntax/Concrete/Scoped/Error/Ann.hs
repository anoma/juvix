module Juvix.Syntax.Concrete.Scoped.Error.Ann where

import Juvix.Syntax.Concrete.Scoped.Pretty.Base qualified as P

data Eann
  = Highlight
  | ScopedAnn P.Ann
