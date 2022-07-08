module Juvix.Syntax.MicroJuvix.Error.Pretty.Ann where

import Juvix.Syntax.MicroJuvix.Pretty.Ann qualified as Micro

data Eann
  = Highlight
  | MicroAnn Micro.Ann
