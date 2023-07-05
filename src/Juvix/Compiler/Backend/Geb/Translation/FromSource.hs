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
import Text.Megaparsec.Char.Lexer qualified as P

data LispDefParameter = LispDefParameter
  { _lispDefParameterName :: Text,
    _lispDefParameterMorphism :: Geb.Morphism
  }

makeLenses ''LispDefParameter

fromSource ::
  Member (Error JuvixError) r =>
  Path Abs File ->
  Text ->
  Sem r Geb.Expression
fromSource fileName input =
  case runParser fileName input of
    Left err -> throw (JuvixError err)
    Right gebTerm -> pure gebTerm

runParser ::
  Path Abs File ->
  Text ->
  Either MegaparsecError Geb.Expression
runParser fileName input = do
  let parser :: ParsecS r Geb.Expression
      parser
        | isJuvixGebFile fileName = parseGeb
        | isLispFile fileName = parseGebLisp
        | otherwise = error "unknown file extension"
  case run $
    P.runParserT parser (fromAbsFile fileName) input of
    Left err -> Left (MegaparsecError err)
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

parseTypedMorphism :: ParsecS r Geb.TypedMorphism
parseTypedMorphism =
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
      m <- morphism
      return
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

parseGeb :: ParsecS r Geb.Expression
parseGeb =
  P.label "<geb program>" $ do
    space
    ( P.try (Geb.ExpressionObject <$> object)
        <|> P.try (Geb.ExpressionMorphism <$> morphism)
        <|> Geb.ExpressionTypedMorphism <$> parseTypedMorphism
      )
      <* P.eof

morphism :: ParsecS r Geb.Morphism
morphism =
  P.label "<geb morphism>" $ do
    morphismUnit
      <|> Geb.MorphismInteger <$> morphismInteger
      <|> parens
        ( morphismUnit
            <|> Geb.MorphismAbsurd <$> morphismAbsurd
            <|> Geb.MorphismLeft <$> morphismLeftInj
            <|> Geb.MorphismRight <$> morphismRightInj
            <|> Geb.MorphismCase <$> morphismCase
            <|> Geb.MorphismPair <$> morphismPair
            <|> Geb.MorphismFirst <$> morphismFirst
            <|> Geb.MorphismSecond <$> morphismSecond
            <|> Geb.MorphismLambda <$> morphismLambda
            <|> Geb.MorphismApplication <$> morphismApplication
            <|> Geb.MorphismVar <$> morphismVar
            <|> Geb.MorphismBinop <$> morphismBinop
            <|> Geb.MorphismFail <$> morphismFail
        )

morphismList :: ParsecS r Geb.Morphism
morphismList = parens $ do
  kw kwList
  morphism

parseNatural :: ParsecS r Integer
parseNatural = lexeme P.decimal

morphismInteger :: ParsecS r Integer
morphismInteger = parseNatural

opcode :: ParsecS r Geb.Opcode
opcode =
  P.label "<geb Opcode>" $
    Geb.OpAdd <$ kw kwGebBinopAdd
      <|> Geb.OpSub <$ kw kwGebBinopSub
      <|> Geb.OpMul <$ kw kwGebBinopMul
      <|> Geb.OpDiv <$ kw kwGebBinopDiv
      <|> Geb.OpMod <$ kw kwGebBinopMod
      <|> Geb.OpEq <$ kw kwGebBinopEq
      <|> Geb.OpLt <$ kw kwGebBinopLt

morphismBinop :: ParsecS r Geb.Binop
morphismBinop = do
  P.label "<geb MorphismBinop>" $ do
    op <- opcode
    m1 <- morphism
    m2 <- morphism
    return
      Geb.Binop
        { _binopOpcode = op,
          _binopLeft = m1,
          _binopRight = m2
        }

morphismFail :: ParsecS r Geb.Failure
morphismFail = do
  P.label "<geb MorphismFail>" $ do
    kw kwFail
    msg <- fst <$> string
    Geb.Failure msg <$> object

object :: ParsecS r Geb.Object
object =
  P.label "<geb Object>" $ do
    objectInitial
      <|> objectTerminal
      <|> Geb.ObjectInteger <$ (kw kwGebObjectInteger)
      <|> parens
        ( Geb.ObjectProduct <$> objectProduct
            <|> Geb.ObjectCoproduct <$> objectCoproduct
            <|> Geb.ObjectHom <$> objectHom
        )

objectList :: ParsecS r Geb.Object
objectList = parens $ do
  kw kwList
  object

morphismUnit :: ParsecS r Geb.Morphism
morphismUnit = do
  P.label "<geb MorphismUnit>" $ do
    kw kwGebMorphismUnit
    return Geb.MorphismUnit

morphismAbsurd :: ParsecS r Geb.Absurd
morphismAbsurd =
  P.label "<geb MorphismAbsurd>" $ do
    kw kwGebMorphismAbsurd
    obj <- object
    morph <- morphism
    return $
      Geb.Absurd
        { _absurdType = obj,
          _absurdValue = morph
        }

morphismLeftInj :: ParsecS r Geb.LeftInj
morphismLeftInj = do
  P.label "<geb MorphismLeft>" $ do
    kw kwGebMorphismLeft
    rType <- object
    lValue <- morphism
    return $
      Geb.LeftInj
        { _leftInjRightType = rType,
          _leftInjValue = lValue
        }

morphismRightInj :: ParsecS r Geb.RightInj
morphismRightInj = do
  P.label "<geb MorphismRight>" $ do
    kw kwGebMorphismRight
    lType <- object
    rValue <- morphism
    return $
      Geb.RightInj
        { _rightInjLeftType = lType,
          _rightInjValue = rValue
        }

morphismCase :: ParsecS r Geb.Case
morphismCase = do
  P.label "<geb MorphismCase>" $ do
    kw kwGebMorphismCase
    _caseOn <- morphism
    _caseLeft <- morphism
    _caseRight <- morphism
    return Geb.Case {..}

morphismPair :: ParsecS r Geb.Pair
morphismPair = do
  P.label "<geb MorphismPair>" $ do
    kw kwGebMorphismPair
    _pairLeft <- morphism
    _pairRight <- morphism
    return Geb.Pair {..}

morphismFirst :: ParsecS r Geb.First
morphismFirst = do
  P.label "<geb MorphismFirst>" $ do
    kw kwGebMorphismFirst
    _firstValue <- morphism
    return Geb.First {..}

morphismSecond :: ParsecS r Geb.Second
morphismSecond = do
  P.label "<geb MorphismSecond>" $ do
    kw kwGebMorphismSecond
    _secondValue <- morphism
    return Geb.Second {..}

morphismLambda :: ParsecS r Geb.Lambda
morphismLambda = do
  P.label "<geb MorphismLambda>" $ do
    kw kwGebMorphismLambda
    _lambdaVarType <- objectList
    _lambdaBody <- morphism
    return Geb.Lambda {..}

morphismApplication :: ParsecS r Geb.Application
morphismApplication = do
  P.label "<geb MorphismApplication>" $ do
    kw kwGebMorphismApplication
    _applicationLeft <- morphism
    _applicationRight <- morphismList
    return Geb.Application {..}

morphismVar :: ParsecS r Geb.Var
morphismVar = do
  P.label "<geb MorphismVar>" $ do
    kw kwGebVar <* space
    _varIndex <- fromIntegral <$> parseNatural
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
