module MiniJuvix.Syntax.MicroJuvix.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.Pretty.Base qualified as Micro

ppCode :: Micro.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate MicroAnn . Micro.runPrettyCode Micro.defaultOptions

prettyError :: PrettyError e => e -> AnsiText
prettyError = AnsiText . PPOutput . (<> "ת") . ppError

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

newtype PPOutput = PPOutput (Doc Eann)

indent' :: Doc ann -> Doc ann
indent' = indent 2

prettyT :: Text -> Doc Eann
prettyT = pretty

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

class PrettyError e where
  ppError :: e -> Doc Eann

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

instance PrettyError WrongConstructorType where
  ppError e =
    "The constructor" <+> ppCode (e ^. wrongCtorTypeName) <+> "has type:"
      <> line
      <> indent' (ppCode (e ^. wrongCtorTypeActual))
      <> line
      <> "but is expected to have type:"
      <> line
      <> indent' (ppCode (e ^. wrongCtorTypeExpected))

instance ToGenericError WrongConstructorType where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. wrongCtorTypeName)

-- | the arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [FunctionArgType],
    _wrongCtorAppName :: Name
  }
  deriving stock (Show)

makeLenses ''WrongConstructorAppArgs

instance PrettyError WrongConstructorAppArgs where
  ppError e =
    "The constructor:" <+> ctorName <+> "is being matched against" <+> numPats
      <> ":"
      <> line
      <> indent' (ppCode (e ^. wrongCtorAppApp))
      <> line
      <> "but is expected to be matched against" <+> numTypes <+> "with the following types:"
      <> line
      <> indent' (hsep (ctorName : (ppCode <$> (e ^. wrongCtorAppTypes))))
    where
      numPats :: Doc ann
      numPats = pat (length (e ^. wrongCtorAppApp . constrAppParameters))
      numTypes :: Doc ann
      numTypes = pat (length (e ^. wrongCtorAppTypes))
      ctorName :: Doc Eann
      ctorName = ppCode (e ^. wrongCtorAppApp . constrAppConstructor)
      pat :: Int -> Doc ann
      pat n = pretty n <+> plural "pattern" "patterns" n

instance ToGenericError WrongConstructorAppArgs where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. wrongCtorAppApp . constrAppConstructor)

-- | the type of an expression does not match the inferred type
data WrongType = WrongType
  { _wrongTypeExpression :: Expression,
    _wrongTypeExpectedType :: Type,
    _wrongTypeInferredType :: Type
  }
  deriving stock (Show)

makeLenses ''WrongType

instance PrettyError WrongType where
  ppError e =
    "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
      <> line
      <> "The expression" <+> ppCode subjectExpr <+> "has type:"
      <> line
      <> indent' (ppCode (e ^. wrongTypeInferredType))
      <> line
      <> "but is expected to have type:"
      <> line
      <> indent' (ppCode (e ^. wrongTypeExpectedType))
    where
      subjectExpr :: Expression
      subjectExpr = e ^. wrongTypeExpression

instance ToGenericError WrongType where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. wrongTypeExpression)

-- | The left hand expression of a function application is not
-- a function type.
data ExpectedFunctionType = ExpectedFunctionType
  { _expectedFunctionTypeExpression :: Expression,
    _expectedFunctionTypeApp :: Expression,
    _expectedFunctionTypeType :: Type
  }
  deriving stock (Show)

makeLenses ''ExpectedFunctionType

instance PrettyError ExpectedFunctionType where
  ppError e =
    "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
      <> line
      <> "In the expression:"
      <> line
      <> indent' (ppCode (e ^. expectedFunctionTypeExpression))
      <> line
      <> "the expression" <+> ppCode (e ^. expectedFunctionTypeApp) <+> "is expected to have a function type but has type:"
      <> line
      <> indent' (ppCode (e ^. expectedFunctionTypeType))
    where
      subjectExpr :: Expression
      subjectExpr = e ^. expectedFunctionTypeExpression

instance ToGenericError ExpectedFunctionType where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. expectedFunctionTypeExpression)

-- | A function definition clause matches too many arguments
data TooManyPatterns = TooManyPatterns
  { _tooManyPatternsClause :: FunctionClause,
    _tooManyPatternsTypes :: [FunctionArgType]
  }
  deriving stock (Show)

makeLenses ''TooManyPatterns

instance PrettyError TooManyPatterns where
  ppError e =
    "Type error near" <+> highlight (pretty (name ^. nameDefined))
      <> line
      <> "In in the definition of" <+> ppCode name <+> "the function clause:"
      <> line
      <> indent' (ppCode (e ^. tooManyPatternsClause))
      <> line
      <> "matches too many patterns. It should match the following types:"
      <> line
      <> indent' (hsep (ppCode <$> (e ^. tooManyPatternsTypes)))
    where
      name :: Name
      name = e ^. tooManyPatternsClause . clauseName

instance ToGenericError TooManyPatterns where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. tooManyPatternsClause . clauseName)
