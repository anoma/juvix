module Juvix.Termination.Error.Pretty.Ann where

import Juvix.Syntax.Abstract.Pretty.Ann

data Eann
  = Highlight
  | AbstractAnn Ann
