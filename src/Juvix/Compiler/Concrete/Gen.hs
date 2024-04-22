module Juvix.Compiler.Concrete.Gen
  ( module Juvix.Compiler.Concrete.Gen,
    module Juvix.Compiler.Concrete.Keywords,
  )
where

import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language
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

symbol :: (Member (Reader Interval) r) => Text -> Sem r Symbol
symbol t = do
  l <- ask
  return (WithLoc l t)

expressionAtoms' :: (Member (Reader Interval) r) => NonEmpty (ExpressionAtom 'Parsed) -> Sem r (ExpressionAtoms 'Parsed)
expressionAtoms' _expressionAtoms = do
  _expressionAtomsLoc <- Irrelevant <$> ask
  return ExpressionAtoms {..}

namedArgument :: (Member (Reader Interval) r) => Text -> NonEmpty (ExpressionAtom 'Parsed) -> Sem r (NamedArgument 'Parsed)
namedArgument n as = do
  _namedArgValue <- expressionAtoms' as
  _namedArgName <- symbol n
  _namedArgAssignKw <- Irrelevant <$> kw kwAssign
  return NamedArgument {..}

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

argumentBlock :: (Member (Reader Interval) r) => IsImplicit -> NonEmpty (NamedArgument 'Parsed) -> Sem r (ArgumentBlock 'Parsed)
argumentBlock i as = do
  parenL <- kw delimL
  parenR <- kw delimR
  return
    ArgumentBlock
      { _argBlockImplicit = i,
        _argBlockDelims = Irrelevant (Just (parenL, parenR)),
        _argBlockArgs = as
      }
  where
    delimL :: Keyword
    delimL = case i of
      Explicit -> kwBracketL
      Implicit -> delimBraceL
      ImplicitInstance -> delimDoubleBraceL

    delimR :: Keyword
    delimR = case i of
      Explicit -> kwBracketR
      Implicit -> delimBraceR
      ImplicitInstance -> delimDoubleBraceR

namedApplication :: Name -> NonEmpty (ArgumentBlock 'Parsed) -> ExpressionAtom 'Parsed
namedApplication n as =
  AtomNamedApplication
    NamedApplication
      { _namedAppName = n,
        _namedAppArgs = as
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

functionDefExpression :: (Member (Reader Interval) r) => NonEmpty (ExpressionAtom 'Parsed) -> Sem r (FunctionDefBody 'Parsed)
functionDefExpression exp = SigBodyExpression <$> expressionAtoms' exp
