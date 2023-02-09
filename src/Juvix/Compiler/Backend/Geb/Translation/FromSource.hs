module Juvix.Compiler.Backend.Geb.Translation.FromSource
  ( module Juvix.Compiler.Backend.Geb.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Keywords
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Juvix.Prelude
import Text.Megaparsec qualified as P

data LispDefParameter = LispDefParameter
  { _lispDefParameterName :: Text,
    _lispDefParameterMorphism :: Geb.TypedMorphism
  }

makeLenses ''LispDefParameter

fromSource ::
  Member (Error JuvixError) r =>
  Path Abs File ->
  Text ->
  Sem r Geb.Expression
fromSource fileName input =
  case runParser fileName input of
    Left err -> throw err
    Right gebTerm -> pure gebTerm

runParser ::
  Path Abs File ->
  Text ->
  Either JuvixError Geb.Expression
runParser fileName input =
  do
    let parser :: ParsecS r Geb.Expression
        parser
          | isJuvixGebFile fileName = parseGeb
          | isLispFile fileName = parseGebLisp
          | otherwise = error "unknown file extension"
    case run $
      P.runParserT parser (fromAbsFile fileName) input of
      Left err -> Left . JuvixError $ ErrMegaparsec (MegaparsecError err)
      Right gebTerm -> Right gebTerm

parseLispSymbol :: ParsecS r Text
parseLispSymbol =
  P.label "<lisp symbol>" $ do
    lexeme $
      P.takeWhile1P Nothing validChars
  where
    validChars :: Char -> Bool
    validChars = (`notElem` ("() " :: String))

parseLispList :: ParsecS r ()
parseLispList =
  P.label "<lisp list>" $
    lexeme . parens $
      P.skipSome parseLispExpr

parseLispExpr :: ParsecS r ()
parseLispExpr =
  void parseLispSymbol
    <|> parseLispList

parseTypeMorphism :: ParsecS r Geb.TypedMorphism
parseTypeMorphism =
  parens $ do
    symbol "typed"
    m <- morphism
    o <- object
    return $
      Geb.TypedMorphism
        { _typedMorphism = m,
          _typedMorphismObject = o
        }

parseDefParameter :: ParsecS r LispDefParameter
parseDefParameter =
  P.label "<defparameter>" $ do
    parens $ do
      symbol "defparameter"
      n <- parseLispSymbol
      m <- parseTypeMorphism
      return $
        LispDefParameter
          { _lispDefParameterName = n,
            _lispDefParameterMorphism = m
          }

parseGebLisp :: ParsecS r Geb.Expression
parseGebLisp = do
  space
  P.label "<defpackage>" parseLispExpr
  P.label "<in-package>" parseLispExpr
  entry <- parseDefParameter
  P.eof
  return $
    Geb.ExpressionMorphism $
      entry
        ^. lispDefParameterMorphism
        . Geb.typedMorphism

parseGebExpression :: ParsecS r Geb.Expression
parseGebExpression =
  P.try (Geb.ExpressionObject <$> object)
    <|> Geb.ExpressionMorphism <$> morphism

parseGeb :: ParsecS r Geb.Expression
parseGeb =
  P.label "<geb program>" $
    space *> parseGebExpression <* P.eof

morphism :: ParsecS r Geb.Morphism
morphism =
  P.label "<geb morphism>" $ do
    morphismUnit
      <|> parens
        ( Geb.MorphismAbsurd <$> morphismAbsurd
            <|> Geb.MorphismLeft <$> morphismLeft
            <|> Geb.MorphismRight <$> morphismRight
            <|> Geb.MorphismCase <$> morphismCase
            <|> Geb.MorphismPair <$> morphismPair
            <|> Geb.MorphismFirst <$> morphismFirst
            <|> Geb.MorphismSecond <$> morphismSecond
            <|> Geb.MorphismLambda <$> morphismLambda
            <|> Geb.MorphismApplication <$> morphismApplication
            <|> Geb.MorphismVar <$> morphismVar
        )

object :: ParsecS r Geb.Object
object =
  P.label "<geb Object>" $ do
    objectInitial
      <|> objectTerminal
      <|> parens
        ( Geb.ObjectProduct <$> objectProduct
            <|> Geb.ObjectCoproduct <$> objectCoproduct
            <|> Geb.ObjectHom <$> objectHom
        )

morphismUnit :: ParsecS r Geb.Morphism
morphismUnit = do
  P.label "<geb MorphismUnit>" $ do
    kw kwGebMorphismUnit
    return Geb.MorphismUnit

morphismAbsurd :: ParsecS r Geb.Morphism
morphismAbsurd =
  P.label "<geb MorphismAbsurd>" $ do
    kw kwGebMorphismAbsurd
    morphism

morphismLeft :: ParsecS r Geb.Morphism
morphismLeft = do
  P.label "<geb MorphismLeft>" $ do
    kw kwGebMorphismLeft
    morphism

morphismRight :: ParsecS r Geb.Morphism
morphismRight = do
  P.label "<geb MorphismRight>" $ do
    kw kwGebMorphismRight
    morphism

morphismCase :: ParsecS r Geb.Case
morphismCase = do
  P.label "<geb MorphismCase>" $ do
    kw kwGebMorphismCase
    _caseLeftType <- object
    _caseRightType <- object
    _caseCodomainType <- object
    _caseOn <- morphism
    _caseLeft <- morphism
    _caseRight <- morphism
    return Geb.Case {..}

morphismPair :: ParsecS r Geb.Pair
morphismPair = do
  P.label "<geb MorphismPair>" $ do
    kw kwGebMorphismPair
    _pairLeftType <- object
    _pairRightType <- object
    _pairLeft <- morphism
    _pairRight <- morphism
    return Geb.Pair {..}

morphismFirst :: ParsecS r Geb.First
morphismFirst = do
  P.label "<geb MorphismFirst>" $ do
    kw kwGebMorphismFirst
    _firstLeftType <- object
    _firstRightType <- object
    _firstValue <- morphism
    return Geb.First {..}

morphismSecond :: ParsecS r Geb.Second
morphismSecond = do
  P.label "<geb MorphismSecond>" $ do
    kw kwGebMorphismSecond
    _secondLeftType <- object
    _secondRightType <- object
    _secondValue <- morphism
    return Geb.Second {..}

morphismLambda :: ParsecS r Geb.Lambda
morphismLambda = do
  P.label "<geb MorphismLambda>" $ do
    kw kwGebMorphismLambda
    _lambdaVarType <- object
    _lambdaBodyType <- object
    _lambdaBody <- morphism
    return Geb.Lambda {..}

morphismApplication :: ParsecS r Geb.Application
morphismApplication = do
  P.label "<geb MorphismApplication>" $ do
    kw kwGebMorphismApplication
    _applicationDomainType <- object
    _applicationCodomainType <- object
    _applicationLeft <- morphism
    _applicationRight <- morphism
    return Geb.Application {..}

morphismVar :: ParsecS r Geb.Var
morphismVar = do
  P.label "<geb MorphismVar>" $ do
    kw kwGebVar <* space
    _varIndex <- fromIntegral . fst <$> integer
    return Geb.Var {..}

objectInitial :: ParsecS r Geb.Object
objectInitial = do
  P.label "objectInitial>" $ do
    kw kwGebObjectInitial
    return Geb.ObjectInitial

objectTerminal :: ParsecS r Geb.Object
objectTerminal = do
  P.label "objectTermina>" $ do
    kw kwGebObjectTerminal
    return Geb.ObjectTerminal

objectProduct :: ParsecS r Geb.Product
objectProduct = do
  P.label "objectProduct>" $ do
    kw kwGebObjectProduct
    _productLeft <- object
    _productRight <- object
    return Geb.Product {..}

objectCoproduct :: ParsecS r Geb.Coproduct
objectCoproduct = do
  P.label "objectCoproduct>" $ do
    kw kwGebObjectCoproduct
    _coproductLeft <- object
    _coproductRight <- object
    return Geb.Coproduct {..}

objectHom :: ParsecS r Geb.Hom
objectHom = do
  P.label "objectHom >" $ do
    kw kwGebObjectHom
    _homDomain <- object
    _homCodomain <- object
    return Geb.Hom {..}
