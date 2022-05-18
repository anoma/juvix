module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann where

import MiniJuvix.Syntax.MicroJuvix.Pretty.Ann qualified as Micro

data Eann
  = Highlight
  | MicroAnn Micro.Ann
