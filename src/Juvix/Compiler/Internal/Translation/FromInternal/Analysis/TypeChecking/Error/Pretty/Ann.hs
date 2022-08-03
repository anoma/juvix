module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty.Ann where

import Juvix.Compiler.Internal.Pretty.Ann qualified as Micro

data Eann
  = Highlight
  | MicroAnn Micro.Ann
