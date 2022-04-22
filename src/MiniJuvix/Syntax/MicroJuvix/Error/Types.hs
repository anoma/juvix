module MiniJuvix.Syntax.MicroJuvix.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: InductiveName,
    _wrongCtorTypeActual :: InductiveName,
    _wrongCtorTypeFunname :: Name
  }
  deriving stock (Show)

-- | the arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [FunctionArgType],
    _wrongCtorAppName :: Name
  }
  deriving stock (Show)

-- | the type of an expression does not match the inferred type
data WrongType = WrongType
  { _wrongTypeExpression :: Expression,
    _wrongTypeExpectedType :: Type,
    _wrongTypeInferredType :: Type
  }
  deriving stock (Show)

-- | The left hand expression of a function application is not
-- a function type.
data ExpectedFunctionType = ExpectedFunctionType
  { _expectedFunctionTypeExpression :: Expression,
    _expectedFunctionTypeApp :: Expression,
    _expectedFunctionTypeType :: Type
  }
  deriving stock (Show)

-- | A function definition clause matches too many arguments
data TooManyPatterns = TooManyPatterns
  { _tooManyPatternsClause :: FunctionClause,
    _tooManyPatternsTypes :: [FunctionArgType]
  }
  deriving stock (Show)

makeLenses ''WrongConstructorType
makeLenses ''WrongConstructorAppArgs
makeLenses ''WrongType
makeLenses ''ExpectedFunctionType
makeLenses ''TooManyPatterns
