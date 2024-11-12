module Juvix.Compiler.Nockma.Highlight.Doc.Parser.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Highlight.Doc.Base
import Juvix.Compiler.Nockma.Language (atomOps)
import Juvix.Parser.Error.Base
import Juvix.Parser.Lexer
import Juvix.Prelude
import Juvix.Prelude.Parsing as P
import Text.Megaparsec.Char.Lexer (decimal)

type Parse a = Parsec Void Text a

parseRules :: FilePath -> Text -> Either MegaparsecError Rules
parseRules fp = mapLeft MegaparsecError . runParser (top pRules) fp

lexeme :: Parse a -> Parse a
lexeme m = m <* whiteSpace

top :: Parse a -> Parse a
top p = whiteSpace >> p <* eof

pTermSymbol :: Parse TermSymbol
pTermSymbol = lexeme $ do
  l <- satisfy validLetter
  subscript <- optional decimal
  ps <- length <$> P.many (chunk "'")
  return
    TermSymbol
      { _termSymbolLetter = l,
        _termSymbolSubscript = subscript,
        _termSymbolPrimes = fromIntegral ps
      }
  where
    -- the letter 's' is reserved for the stack
    validLetter :: Char -> Bool
    validLetter = isLetter .&&. (not . isStackSymbol)

isStackSymbol :: Char -> Bool
isStackSymbol = (== 's') . toLower

pPathSymbol :: Parse PathSymbol
pPathSymbol =
  lexeme $
    chunk "p" $> PathP

pStack :: Parse Symbol
pStack = lexeme $ satisfy isStackSymbol $> SymbolStack

pNockOp :: Parse NockOp
pNockOp =
  lexeme $
    choice
      [ chunk (opName <> " ") $> op
        | (opName, op) <- HashMap.toList atomOps
      ]

kw :: Keyword -> Parse ()
kw k =
  lexeme
    . void
    . choice
    . map chunk
    $ (k ^. keywordAscii)
      : maybeToList (k ^. keywordUnicode)

delims :: Keyword -> Keyword -> Parse a -> Parse a
delims l r p = do
  kw l
  p <* kw r

brackets :: Parse a -> Parse a
brackets = delims delimBracketL delimBracketR

parens :: Parse a -> Parse a
parens = delims delimParenL delimParenR

pReplace :: Parse Replace
pReplace = do
  kw kwReplace
  parens $ do
    _replaceBase <- pTerm
    kw delimSemicolon
    _replacePath <- pPathSymbol
    kw delimSemicolon
    _replaceBy <- pTerm
    optional (kw delimSemicolon)
    return Replace {..}

pIndexAt :: Parse IndexAt
pIndexAt = do
  kw kwIndex
  parens $ do
    _indexAtBase <- pTerm
    kw delimSemicolon
    _indexAtPath <- pPathSymbol
    return IndexAt {..}

pSuccessor :: Parse Successor
pSuccessor = do
  kw kwSuc
  t <- parens pTerm
  return
    Successor
      { _successor = t
      }

pZero :: Parse ()
pZero = lexeme . void $ chunk "0"

pOne :: Parse ()
pOne = lexeme . void $ chunk "1"

pAtom :: Parse Atom
pAtom =
  choice
    [ AtomOperator <$> pNockOp,
      AtomNotation <$> pNotation,
      AtomZero <$ pZero,
      AtomOne <$ pOne,
      AtomSymbol <$> pSymbol
    ]

pSymbol :: Parse Symbol
pSymbol =
  choice
    [ SymbolStack <$ pStack,
      SymbolPath <$> pPathSymbol,
      SymbolTerm <$> pTermSymbol
    ]

pNotation :: Parse Notation
pNotation =
  choice
    [ NotationIndex <$> pIndexAt,
      NotationReplace <$> pReplace,
      NotationSuccessor <$> pSuccessor
    ]

pNeq :: Parse Neq
pNeq = do
  kw kwNeq
  parens $ do
    _neqLhs <- pTerm
    kw delimSemicolon
    _neqRhs <- pTerm
    optional (kw delimSemicolon)
    return Neq {..}

pCell :: Parse Cell
pCell = brackets $ do
  _cellLhs <- pTerm
  _cellRhs <- pTerm
  return Cell {..}

pTerm :: Parse Term
pTerm =
  TermCell <$> pCell
    <|> TermAtom <$> pAtom

pContext :: Parse Context
pContext = do
  _contextLhs <- pTerm
  kw kwStar
  _contextRhs <- pTerm
  return Context {..}

pEvalRelation :: Parse EvalRelation
pEvalRelation = do
  _evalContext <- pContext
  kw kwDoubleArrowR
  _evalRhs <- pTerm
  return EvalRelation {..}

pRelation :: Parse Relation
pRelation =
  choice
    [ RelationNeq <$> pNeq,
      RelationEval <$> pEvalRelation
    ]

pRule :: Parse Rule
pRule = do
  _ruleConditions <- sepEndBy pRelation (kw kwNockmaLogicAnd)
  kw delimRule
  _rulePost <- pEvalRelation
  return Rule {..}

pRules :: Parse Rules
pRules = Rules <$> sepEndBy1 pRule (kw kwAnd)
