{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Nockma.Highlight.Doc.Pretty () where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Juvix.Compiler.Nockma.Highlight.Doc.Base
import Juvix.Data.CodeAnn
import Juvix.Prelude

data ColorCounter :: Effect where
  GetSymbolColor :: Symbol -> ColorCounter m CodeAnn

makeSem ''ColorCounter

getTermSymbolColor :: (Members '[ColorCounter] r) => TermSymbol -> Sem r CodeAnn
getTermSymbolColor = getSymbolColor . SymbolTerm

getPathColor :: (Members '[ColorCounter] r) => PathSymbol -> Sem r CodeAnn
getPathColor = getSymbolColor . SymbolPath

getStackColor :: (Members '[ColorCounter] r) => Sem r CodeAnn
getStackColor = getSymbolColor SymbolStack

runColorCounter :: Sem (ColorCounter ': r) a -> Sem r a
runColorCounter = reinterpret (evalState (mempty :: HashMap TermSymbol CodeAnn)) $ \case
  -- GetPathColor p -> case p of
  --   PathP -> return AnnLiteralString
  -- GetStackColor ->
  GetSymbolColor s -> case s of
    SymbolStack -> return (AnnKind KNameLocal)
    SymbolPath p -> case p of
      PathP -> return AnnLiteralString
    SymbolTerm sym -> do
      tbl <- get @(HashMap TermSymbol CodeAnn)
      let m = length tbl
      case tbl ^. at sym of
        Just c -> return c
        Nothing -> do
          let color = colorAt m
          modify (HashMap.insert sym color)
          return color
      where
        colorAt :: Int -> CodeAnn
        colorAt i = colors !! (i `mod` n)

        n :: Int
        n
          | notNull colors = length colors
          | otherwise = impossibleError "there must be at least one color"

        colors :: [CodeAnn]
        colors =
          AnnKind
            <$> [ KNameConstructor,
                  KNameInductive,
                  KNameAxiom,
                  KNameTopModule
                ]

type PP a = a -> Sem '[ColorCounter] (Doc CodeAnn)

instance PrettyCodeAnn Rules where
  ppCodeAnn = run . runColorCounter . ppRules

ppNotation :: PP Notation
ppNotation = \case
  NotationReplace r -> ppReplace r
  NotationIndex i -> ppIndexAt i
  NotationSuccessor i -> ppSuccessor i

ppAtom :: PP Atom
ppAtom = \case
  AtomSymbol s -> ppSymbol s
  AtomOperator n -> ppOperator n
  AtomNotation n -> ppNotation n
  AtomZero -> return (annotate AnnKeyword "0")
  AtomOne -> return (annotate AnnKeyword "1")

ppSymbol :: PP Symbol
ppSymbol = \case
  SymbolStack -> ppStack
  SymbolTerm t -> ppTermSymbol t
  SymbolPath p -> ppPathSymbol p

ppTermSymbol :: PP TermSymbol
ppTermSymbol s@TermSymbol {..} = do
  c <- getTermSymbolColor s
  let primes = Text.replicate (fromIntegral _termSymbolPrimes) "'"
      sym =
        Text.singleton _termSymbolLetter
          <>? (unicodeSubscript <$> _termSymbolSubscript)
          <> primes
  return
    . annotate c
    . pretty
    $ sym

ppOperator :: PP NockOp
ppOperator = return . ppCodeAnn

ppReplace :: PP Replace
ppReplace Replace {..} = do
  b' <- ppTerm _replaceBase
  ix' <- ppPathSymbol _replacePath
  by' <- ppTerm _replaceBy
  return $
    b'
      <> ppCodeAnn delimBraceL
      <> ix'
      <+> ppCodeAnn kwMapsTo
      <+> by'
        <> ppCodeAnn delimBraceR

ppPathSymbol :: PP PathSymbol
ppPathSymbol = \case
  p@PathP -> do
    c <- getPathColor p
    return (annotate c "p")

ppNeq :: PP Neq
ppNeq Neq {..} = do
  l' <- ppTerm _neqLhs
  r' <- ppTerm _neqRhs
  return $
    l' <+> ppCodeAnn kwNeqSymbol <+> r'

ppIndexAt :: PP IndexAt
ppIndexAt IndexAt {..} = do
  b' <- ppTerm _indexAtBase
  ix' <- ppPathSymbol _indexAtPath
  return $
    b' <+> ppCodeAnn kwExclamation <+> ix'

ppStack :: (Members '[ColorCounter] r) => Sem r (Doc CodeAnn)
ppStack = do
  c <- getStackColor
  return (annotate c "S")

ppSuccessor :: PP Successor
ppSuccessor Successor {..} = do
  t' <- ppTerm _successor
  return $
    t' <+> "+" <+> "1"

ppCell :: PP Cell
ppCell Cell {..} = do
  l <- ppTerm _cellLhs
  r <- ppTerm _cellRhs
  return $
    ppCodeAnn delimBracketL
      <> l
      <+> r
        <> ppCodeAnn delimBracketR

ppTerm :: PP Term
ppTerm = \case
  TermAtom a -> ppAtom a
  TermCell a -> ppCell a

ppEvalRelation :: PP EvalRelation
ppEvalRelation EvalRelation {..} = do
  ctx' <- ppContext _evalContext
  r' <- ppTerm _evalRhs
  return $
    ctx' <+> ppCodeAnn kwDoubleArrowR <+> r'

ppContext :: PP Context
ppContext Context {..} = do
  l <- ppTerm _contextLhs
  r <- ppTerm _contextRhs
  return $
    l <+> ppCodeAnn kwStar <+> r

ppRelation :: PP Relation
ppRelation = \case
  RelationEval r -> ppEvalRelation r
  RelationNeq n -> ppNeq n

ppRule :: PP Rule
ppRule Rule {..} = do
  let sep_ r1 r2 = r1 <+> (" " :: Doc CodeAnn) <+> r2
  conds' <- concatWith sep_ <$> mapM ppRelation _ruleConditions
  post' <- ppEvalRelation _rulePost
  let n1 = Text.length (toPlainText conds')
      n2 = Text.length (toPlainText post')
      hrule = pretty (Text.replicate (max n1 (max n2 3)) "─")
  return $
    conds'
      <> hardline
      <> hrule
      <> hardline
      <> post'

ppRules :: PP Rules
ppRules Rules {..} = do
  rules' <- mapM ppRule _rules
  return $
    concatWith
      ( \r1 r2 ->
          r1
            <> hardline
            <> hardline
            <> ppCodeAnn kwAnd
            <> hardline
            <> hardline
            <> r2
      )
      rules'
