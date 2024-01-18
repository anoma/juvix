module Juvix.Compiler.Tree.Pretty.Options where

import Juvix.Compiler.Tree.Language

data Options = Options
  { _optSymbolNames :: HashMap Symbol Text,
    _optTagNames :: HashMap Tag Text
  }

makeLenses ''Options
