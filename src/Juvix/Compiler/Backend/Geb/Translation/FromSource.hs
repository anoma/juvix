{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Juvix.Compiler.Backend.Geb.Translation.FromSource
  ( module Juvix.Compiler.Backend.Geb.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Keywords
import Juvix.Compiler.Backend.Geb.Language (typedMorphismObject)
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Compiler.Core.Translation.FromSource.Lexer qualified as P
import Juvix.Parser.Error
import Juvix.Prelude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

data GebLispExpr
  = GebLispExprTypedMorphism Geb.TypedMorphism
  | GebLispExprList [GebLispExpr]
  | GebLispExprAtom String
  deriving (Eq, Show)

data LispDefParameter = LispDefParameter
  { _lispDefParameterName :: String,
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

-- Parser for spaces
sc :: ParsecS r ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

-- Parser for a symbol
name :: ParsecS r String
name = P.many (P.letterChar <|> P.digitChar <|> P.char '-' <|> P.char '_')

name' :: ParsecS r String
name' = P.char ':' >> many (name >> P.char '.') >> name <* P.newline

parseLispAtom :: ParsecS r GebLispExpr
parseLispAtom = GebLispExprAtom <$> name

parseLispList :: ParsecS r GebLispExpr
parseLispList =
  GebLispExprList
    <$> P.between lparen rparen (P.many parseList)

parseList :: ParsecS r GebLispExpr
parseList =
  P.try parseLispAtom
    <|> parseLispList

parseTypedMorphism :: ParsecS r Geb.TypedMorphism
parseTypedMorphism =
  P.label "<typed morphism>" $
    P.between lparen rparen $
      do
        symbol "typed"
        t <- morphism
        Geb.TypedMorphism t <$> object

parseDefParameter :: ParsecS r LispDefParameter
parseDefParameter =
  P.label "<defparameter>" $
    P.between lparen rparen $
      do
        symbol "defparameter"
        entryName <- name
        morph :: Geb.TypedMorphism <- parseTypedMorphism
        return
          LispDefParameter
            { _lispDefParameterName = entryName,
              _lispDefParameterMorphism = morph
            }

parseDefPackage :: ParsecS r GebLispExpr
parseDefPackage = do
  symbol "defpackage"
  optional (P.char '#')
  name'
  GebLispExprList <$> many parseLispList

parseGebLisp :: ParsecS r Geb.Expression
parseGebLisp =
  P.label "<lisp geb program>" $ do
    parseDefPackage -- for defpackage
    parseLispList -- for in-package
    entry <- parseDefParameter -- for defparameter
    return $
      Geb.ExpressionMorphism $
        entry
          ^. lispDefParameterMorphism
          . Geb.typedMorphism

{-

(defpackage #:test001
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

docLisp :: Options -> Text -> Text -> Morphism -> Object -> Doc Ann
docLisp opts packageName entryName morph obj =
  "(defpackage #:"
    <> pretty packageName
    <> line
    <> indent' "(:shadowing-import-from :geb.lambda.spec #:func #:pair)"
    <> line
    <> indent' "(:shadowing-import-from :geb.spec #:case)"
    <> line
    <> indent' "(:use #:common-lisp #:geb.lambda.spec #:geb))"
    <> line
    <> line
    <> "(in-package :"
    <> pretty packageName
    <> ")"
    <> line
    <> line
    <> parens
      ( "defparameter"
          <+> pretty entryName
            <> line
            <> indent' (parens ("typed" <+> doc opts morph <+> doc opts obj))
      )
      -}

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
