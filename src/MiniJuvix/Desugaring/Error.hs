module MiniJuvix.Desugaring.Error where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude
import qualified Text.Show

--------------------------------------------------------------------------------

data DesugaringError = DesugaringError
  deriving stock (Show)
