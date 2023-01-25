module Juvix.Compiler.Backend.Geb.Extra where

import Juvix.Compiler.Backend.Geb.Language

destructProd :: Object -> [Object]
destructProd = \case
  ObjectProd Prod {..} -> _prodLeft : destructProd _prodRight
  x -> [x]
