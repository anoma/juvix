module Juvix.Compiler.Internal.Data.TypedIden where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data TypedIden = TypedIden
  { _typedIden :: Iden,
    _typedIdenType :: Expression
  }
  deriving stock (Data, Generic)

makeLenses ''TypedIden
