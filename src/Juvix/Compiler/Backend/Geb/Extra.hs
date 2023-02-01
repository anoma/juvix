module Juvix.Compiler.Backend.Geb.Extra where

import Juvix.Compiler.Backend.Geb.Language

-- | Destructs a product in a right-associative way, e.g., (a, (b, c)) is
-- destructed to [a, b, c]
destructProduct :: Object -> [Object]
destructProduct = \case
  ObjectProduct Product {..} -> _productLeft : destructProduct _productRight
  x -> [x]
