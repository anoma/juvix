module Juvix.Compiler.Concrete.Gen
  ( module Juvix.Compiler.Concrete.Gen,
    module Juvix.Compiler.Concrete.Keywords,
  )
where

import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Prelude

kw :: (Members '[Reader Interval] r) => Keyword -> Sem r KeywordRef
kw k = do
  loc <- ask
  return
    KeywordRef
      { _keywordRefKeyword = k,
        _keywordRefUnicode = Ascii,
        _keywordRefInterval = loc
      }

simplestFunctionDefParsed :: (Member (Reader Interval) r) => Text -> NonEmpty (ExpressionAtom 'Parsed) -> Sem r (FunctionDef 'Parsed)
simplestFunctionDefParsed funNameTxt funBody = do
  funName <- symbol funNameTxt
  return (simplestFunctionDef funName (mkExpressionAtoms funBody))

simplestFunctionDef :: FunctionName s -> ExpressionType s -> FunctionDef s
simplestFunctionDef funName funBody =
  FunctionDef
    { _signName = funName,
      _signBody = SigBodyExpression funBody,
      _signColonKw = Irrelevant Nothing,
      _signArgs = [],
      _signRetType = Nothing,
      _signDoc = Nothing,
      _signPragmas = Nothing,
      _signBuiltin = Nothing,
      _signTerminating = Nothing,
      _signInstance = Nothing,
      _signCoercion = Nothing
    }

smallUniverseExpression :: forall s r. (SingI s) => (Members '[Reader Interval] r) => Sem r (ExpressionType s)
smallUniverseExpression = do
  loc <- ask @Interval
  return $ case sing :: SStage s of
    SScoped -> ExpressionUniverse (smallUniverse loc)
    SParsed ->
      ExpressionAtoms
        { _expressionAtomsLoc = Irrelevant loc,
          _expressionAtoms = pure (AtomUniverse (smallUniverse loc))
        }

isExhaustive :: (Member (Reader Interval) r) => Bool -> Sem r IsExhaustive
isExhaustive _isExhaustive = do
  _isExhaustiveKw <-
    Irrelevant
      <$> if
          | _isExhaustive -> kw kwAt
          | otherwise -> kw kwAtQuestion

  return IsExhaustive {..}

symbol :: (Member (Reader Interval) r) => Text -> Sem r Symbol
symbol t = do
  l <- ask
  return (WithLoc l t)

mkExpressionAtoms :: NonEmpty (ExpressionAtom 'Parsed) -> ExpressionAtoms 'Parsed
mkExpressionAtoms _expressionAtoms =
  ExpressionAtoms
    { _expressionAtoms,
      _expressionAtomsLoc = Irrelevant (getLocSpan _expressionAtoms)
    }

expressionAtoms' :: (Member (Reader Interval) r) => NonEmpty (ExpressionAtom 'Parsed) -> Sem r (ExpressionAtoms 'Parsed)
expressionAtoms' _expressionAtoms = do
  _expressionAtomsLoc <- Irrelevant <$> ask
  return ExpressionAtoms {..}

literalString :: (Member (Reader Interval) r) => Text -> Sem r (ExpressionAtom s)
literalString t = do
  l <- ask
  return (AtomLiteral (WithLoc l (LitString t)))

identifier :: (Member (Reader Interval) r) => Text -> Sem r (ExpressionAtom 'Parsed)
identifier = fmap (AtomIdentifier . NameUnqualified) . symbol

braced :: (Member (Reader Interval) r) => NonEmpty (ExpressionAtom 'Parsed) -> Sem r (ExpressionAtom 'Parsed)
braced a = do
  l <- ask
  AtomBraces . WithLoc l <$> expressionAtoms' a

mkIsExhaustive :: (Member (Reader Interval) r) => Bool -> Sem r IsExhaustive
mkIsExhaustive _isExhaustive = do
  keyw <-
    if
        | _isExhaustive -> kw kwAt
        | otherwise -> kw kwAtQuestion
  return
    IsExhaustive
      { _isExhaustiveKw = Irrelevant keyw,
        _isExhaustive
      }

namedApplication :: Name -> IsExhaustive -> [NamedArgumentNew 'Parsed] -> ExpressionAtom 'Parsed
namedApplication n exh as =
  AtomNamedApplicationNew
    NamedApplicationNew
      { _namedApplicationNewName = n,
        _namedApplicationNewExhaustive = exh,
        _namedApplicationNewArguments = as
      }

literalInteger :: (Member (Reader Interval) r, Integral a) => a -> Sem r (ExpressionAtom 'Parsed)
literalInteger a = do
  l <- ask
  return (AtomLiteral (WithLoc l (LitIntegerWithBase (IntegerWithBase IntegerBaseDecimal (toInteger a)))))

mkList :: (Member (Reader Interval) r) => [NonEmpty (ExpressionAtom 'Parsed)] -> Sem r (ExpressionAtom 'Parsed)
mkList as = do
  items <- mapM expressionAtoms' as
  parenR <- Irrelevant <$> kw kwBracketR
  parenL <- Irrelevant <$> kw kwBracketL
  return
    ( AtomList
        List
          { _listItems = items,
            _listBracketR = parenR,
            _listBracketL = parenL
          }
    )

functionDefExpression ::
  (Member (Reader Interval) r) =>
  NonEmpty (ExpressionAtom 'Parsed) ->
  Sem r (FunctionDefBody 'Parsed)
functionDefExpression exp = SigBodyExpression <$> expressionAtoms' exp
