module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ann where

import Juvix.Compiler.Abstract.Pretty.Ann

data Eann
  = Highlight
  | AbstractAnn Ann
