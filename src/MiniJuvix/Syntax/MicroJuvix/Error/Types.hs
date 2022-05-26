module MiniJuvix.Syntax.MicroJuvix.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Language

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: InductiveName,
    _wrongCtorTypeActual :: InductiveName,
    _wrongCtorTypeFunName :: Name
  }
  deriving stock (Show)

makeLenses ''WrongConstructorType

instance ToGenericError WrongConstructorType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. wrongCtorTypeName)
      msg =
        "The constructor" <+> ppCode (e ^. wrongCtorTypeName) <+> "has type:"
          <> line
          <> indent' (ppCode (e ^. wrongCtorTypeActual))
          <> line
          <> "but is expected to have type:"
          <> line
          <> indent' (ppCode (e ^. wrongCtorTypeExpected))

-- | The arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [FunctionArgType],
    _wrongCtorAppName :: Name
  }
  deriving stock (Show)

makeLenses ''WrongConstructorAppArgs

instance ToGenericError WrongConstructorAppArgs where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. wrongCtorAppApp . constrAppConstructor)
      msg =
        "The constructor:" <+> ctorName <+> "is being matched against" <+> numPats
          <> ":"
          <> line
          <> indent' (ppCode (e ^. wrongCtorAppApp))
          <> line
          <> "but is expected to be matched against" <+> numTypes <+> "with the following types:"
          <> line
          <> indent' (hsep (ctorName : (ppCode <$> (e ^. wrongCtorAppTypes))))
      numPats :: Doc ann
      numPats = pat (length (e ^. wrongCtorAppApp . constrAppParameters))
      numTypes :: Doc ann
      numTypes = pat (length (e ^. wrongCtorAppTypes))

      ctorName :: Doc Eann
      ctorName = ppCode (e ^. wrongCtorAppApp . constrAppConstructor)
      pat :: Int -> Doc ann
      pat n = pretty n <+> plural "pattern" "patterns" n

-- | the type of an expression does not match the inferred type
data WrongType = WrongType
  { _wrongTypeExpression :: Expression,
    _wrongTypeExpectedType :: Type,
    _wrongTypeInferredType :: Type
  }
  deriving stock (Show)

makeLenses ''WrongType

instance ToGenericError WrongType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. wrongTypeExpression)
      msg =
        "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
          <> line
          <> "The expression" <+> ppCode subjectExpr <+> "has type:"
          <> line
          <> indent' (ppCode (e ^. wrongTypeInferredType))
          <> line
          <> "but is expected to have type:"
          <> line
          <> indent' (ppCode (e ^. wrongTypeExpectedType))

      subjectExpr :: Expression
      subjectExpr = e ^. wrongTypeExpression

-- | The left hand expression of a function application is not
-- a function type.
data ExpectedFunctionType = ExpectedFunctionType
  { _expectedFunctionTypeExpression :: Expression,
    _expectedFunctionTypeApp :: Expression,
    _expectedFunctionTypeType :: Type
  }
  deriving stock (Show)

makeLenses ''ExpectedFunctionType

instance ToGenericError ExpectedFunctionType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. expectedFunctionTypeExpression)
      msg =
        "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
          <> line
          <> "In the expression:"
          <> line
          <> indent' (ppCode (e ^. expectedFunctionTypeExpression))
          <> line
          <> "the expression" <+> ppCode (e ^. expectedFunctionTypeApp) <+> "is expected to have a function type but has type:"
          <> line
          <> indent' (ppCode (e ^. expectedFunctionTypeType))
      subjectExpr :: Expression
      subjectExpr = e ^. expectedFunctionTypeExpression

-- | A function definition clause matches too many arguments
data TooManyPatterns = TooManyPatterns
  { _tooManyPatternsClause :: FunctionClause,
    _tooManyPatternsTypes :: [FunctionArgType]
  }
  deriving stock (Show)

makeLenses ''TooManyPatterns

instance ToGenericError TooManyPatterns where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. tooManyPatternsClause . clauseName)

      name :: Name
      name = e ^. tooManyPatternsClause . clauseName

      msg =
        "Type error near" <+> highlight (pretty (name ^. nameDefined))
          <> line
          <> "In in the definition of" <+> ppCode name <+> "the function clause:"
          <> line
          <> indent' (ppCode (e ^. tooManyPatternsClause))
          <> line
          <> "matches too many patterns. It should match the following types:"
          <> line
          <> indent' (hsep (ppCode <$> (e ^. tooManyPatternsTypes)))
