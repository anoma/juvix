module Juvix.Compiler.Backend.Geb.Translation.FromSource
  ( module Juvix.Compiler.Backend.Geb.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Keywords
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Juvix.Prelude
import Text.Megaparsec qualified as P

fromSource ::
  Member (Error ParserError) r =>
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
  Either ParserError Geb.Expression
runParser fileName input =
  case run $
    P.runParserT parseGeb (fromAbsFile fileName) input of
    Left err -> Left $ ErrMegaparsec (MegaparsecError err)
    Right gebTerm -> Right gebTerm

parseGeb :: ParsecS r Geb.Expression
parseGeb =
  P.label "<geb program>" $
    do
      P.try (Geb.ExpressionObject <$> object)
      <|> (Geb.ExpressionMorphism <$> morphism)

morphism :: ParsecS r Geb.Morphism
morphism =
  P.label "<geb morphism>" $
    space *> do
      lparen' <- optional lparen
      ( morphismUnit
          <|> (Geb.MorphismAbsurd <$> morphismAbsurd)
          <|> (Geb.MorphismLeft <$> morphismLeft)
          <|> (Geb.MorphismRight <$> morphismRight)
          <|> (Geb.MorphismCase <$> morphismCase)
          <|> (Geb.MorphismPair <$> morphismPair)
          <|> (Geb.MorphismFirst <$> morphismFirst)
          <|> (Geb.MorphismSecond <$> morphismSecond)
          <|> (Geb.MorphismLambda <$> morphismLambda)
          <|> (Geb.MorphismApplication <$> morphismApplication)
          <|> (Geb.MorphismVar <$> morphismVar)
        )
        <* whenJust lparen' (const rparen)

object :: ParsecS r Geb.Object
object =
  P.label "<geb Object>" $
    space *> do
      lparen' <- optional lparen
      ( objectInitial
          <|> objectTerminal
          <|> (Geb.ObjectProduct <$> objectProduct)
          <|> (Geb.ObjectCoproduct <$> objectCoproduct)
          <|> (Geb.ObjectHom <$> objectHom)
        )
        <* whenJust lparen' (const rparen)

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
