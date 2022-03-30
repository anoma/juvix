module MiniJuvix.Syntax.MicroJuvix.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: Type,
    _wrongCtorTypeActual :: Type
  }

-- | the arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [Type]
  }

makeLenses ''WrongConstructorType
makeLenses ''WrongConstructorAppArgs
