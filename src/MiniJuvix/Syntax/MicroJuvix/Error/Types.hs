module MiniJuvix.Syntax.MicroJuvix.Error.Types where
import MiniJuvix.Syntax.MicroJuvix.Language
import Lens.Micro.Platform (makeLenses)

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: Type,
    _wrongCtorTypeActual :: Type
  }

makeLenses ''WrongConstructorType
