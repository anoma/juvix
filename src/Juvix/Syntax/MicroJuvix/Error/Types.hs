module Juvix.Syntax.MicroJuvix.Error.Types where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.MicroJuvix.Error.Pretty
import Juvix.Syntax.MicroJuvix.Language

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: Name,
    _wrongCtorTypeActual :: Name,
    _wrongCtorTypeFunName :: Name
  }

makeLenses ''WrongConstructorType

instance ToGenericError WrongConstructorType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      ctorName = e ^. wrongCtorTypeName
      i = getLoc ctorName
      msg =
        "The constructor"
          <+> ppCode ctorName
          <+> "belongs to the inductive type:"
            <> line
            <> indent' (ppCode (e ^. wrongCtorTypeActual))
            <> line
            <> "but is expected to belong to the inductive type:"
            <> line
            <> indent' (ppCode (e ^. wrongCtorTypeExpected))

data WrongReturnType = WrongReturnType
  { _wrongReturnTypeConstructorName :: Name,
    _wrongReturnTypeExpected :: Expression,
    _wrongReturnTypeActual :: Expression
  }

makeLenses ''WrongReturnType

instance ToGenericError WrongReturnType where
  genericError e =
    GenericError
      { _genericErrorLoc = j,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i, j]
      }
    where
      ctorName = e ^. wrongReturnTypeConstructorName
      i = getLoc ctorName
      ty = e ^. wrongReturnTypeActual
      j = getLoc ty
      msg =
        "The constructor"
          <+> ppCode ctorName
          <+> "has the wrong return type:"
            <> line
            <> indent' (ppCode ty)
            <> line
            <> "but is expected to have type:"
            <> line
            <> indent' (ppCode (e ^. wrongReturnTypeExpected))

newtype UnsolvedMeta = UnsolvedMeta
  { _unsolvedMeta :: Hole
  }

makeLenses ''UnsolvedMeta

instance ToGenericError UnsolvedMeta where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. unsolvedMeta)
      msg :: Doc a
      msg = "Unable to infer the hole"

-- | The arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [FunctionParameter],
    _wrongCtorAppName :: Name
  }

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
        "The constructor:"
          <+> ctorName
          <+> "is being matched against"
          <+> numPats
            <> ":"
            <> line
            <> indent' (ppCode (e ^. wrongCtorAppApp))
            <> line
            <> "but is expected to be matched against"
          <+> numTypes
          <+> "with the following types:"
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
  { _wrongTypeThing :: Either Expression Pattern,
    _wrongTypeExpected :: Expression,
    _wrongTypeActual :: Expression
  }

makeLenses ''WrongType

instance ToGenericError WrongType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = either getLoc getLoc (e ^. wrongTypeThing)
      msg =
        "The"
          <+> thing
          <+> either ppCode ppCode subjectThing
          <+> "has type:"
            <> line
            <> indent' (ppCode (e ^. wrongTypeActual))
            <> line
            <> "but is expected to have type:"
            <> line
            <> indent' (ppCode (e ^. wrongTypeExpected))
      thing :: Doc a
      thing = case subjectThing of
        Left {} -> "expression"
        Right {} -> "pattern"
      subjectThing :: Either Expression Pattern
      subjectThing = e ^. wrongTypeThing

-- | The left hand expression of a function application is not
-- a function type.
data ExpectedFunctionType = ExpectedFunctionType
  { _expectedFunctionTypeExpression :: Expression,
    _expectedFunctionTypeApp :: Expression,
    _expectedFunctionTypeType :: Expression
  }

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
        "Type error near"
          <+> pretty (getLoc subjectExpr)
            <> "."
            <> line
            <> "In the expression:"
            <> line
            <> indent' (ppCode (e ^. expectedFunctionTypeExpression))
            <> line
            <> "the expression"
          <+> ppCode (e ^. expectedFunctionTypeApp)
          <+> "is expected to have a function type but has type:"
            <> line
            <> indent' (ppCode (e ^. expectedFunctionTypeType))
      subjectExpr :: Expression
      subjectExpr = e ^. expectedFunctionTypeExpression

data WrongNumberArgumentsIndType = WrongNumberArgumentsIndType
  { _wrongNumberArgumentsIndTypeActualType :: Expression,
    _wrongNumberArgumentsIndTypeExpectedNumArgs :: Int,
    _wrongNumberArgumentsIndTypeActualNumArgs :: Int
  }

makeLenses ''WrongNumberArgumentsIndType

instance ToGenericError WrongNumberArgumentsIndType where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      ty = e ^. wrongNumberArgumentsIndTypeActualType
      i = getLoc ty
      expectedNumArgs = e ^. wrongNumberArgumentsIndTypeExpectedNumArgs
      actualNumArgs = e ^. wrongNumberArgumentsIndTypeActualNumArgs
      msg =
        "The type"
          <+> ppCode ty
          <+> "expects"
          <+> ( if
                    | expectedNumArgs == 0 -> "no arguments"
                    | expectedNumArgs == 1 -> "one argument"
                    | otherwise -> pretty expectedNumArgs <+> "arguments"
              )
            <> ", but"
          <+> ( if
                    | actualNumArgs == 0 -> "no argument is"
                    | actualNumArgs == 1 -> "only one argument is"
                    | otherwise -> pretty actualNumArgs <+> "arguments are"
              )
          <+> "given"

newtype ImpracticalPatternMatching = ImpracticalPatternMatching
  { _impracticalPatternMatchingType :: Expression
  }

makeLenses ''ImpracticalPatternMatching

instance ToGenericError ImpracticalPatternMatching where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      ty = e ^. impracticalPatternMatchingType
      i = getLoc ty
      msg =
        "The type"
          <+> ppCode ty
          <+> "is not an inductive data type."
          <+> "Therefore, pattern-matching is not available here"
