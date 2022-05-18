module MiniJuvix.Syntax.Concrete.Scoped.Error.Ann where

import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base qualified as P

data Eann
  = Highlight
  | ScopedAnn P.Ann
