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
  GetColor :: Symbol -> ColorCounter m NameKind

makeSem ''ColorCounter

runColorCounter :: Sem (ColorCounter ': r) a -> Sem r a
runColorCounter = reinterpret (evalState (mempty :: HashMap Symbol NameKind)) $ \case
  GetColor sym -> do
    tbl <- get @(HashMap Symbol NameKind)
    let m = length tbl
    case tbl ^. at sym of
      Just c -> return c
      Nothing -> do
        let color = colorAt m
        modify (HashMap.insert sym color)
        return color
    where
      colorAt :: Int -> NameKind
      colorAt i = colors !! (i `mod` n)

      n :: Int
      n
        | notNull colors = length colors
        | otherwise = impossibleError "there must be at least one color"

      colors :: [NameKind]
      colors =
        [ KNameConstructor,
          KNameInductive,
          KNameFunction,
          KNameTopModule
        ]

type PP a = a -> Sem '[ColorCounter] (Doc CodeAnn)

instance PrettyCodeAnn Rules where
  ppCodeAnn = run . runColorCounter . ppRules

ppAtom :: PP Atom
ppAtom = \case
  AtomSymbol s -> ppSymbol s
  AtomOperator n -> ppOperator n
  AtomReplace r -> ppReplace r
  AtomIndex i -> ppIndexAt i
  AtomStack -> return ppStack
  AtomZero -> return (annotate AnnKeyword "0")
  AtomOne -> return (annotate AnnKeyword "1")
  AtomSuccessor s -> ppSuccessor s

ppSymbol :: PP Symbol
ppSymbol s@Symbol {..} = do
  c <- getColor s
  let primes = Text.replicate (fromIntegral _symbolPrimes) "'"
      sym =
        Text.singleton _symbolLetter
          <>? (unicodeSubscript <$> _symbolSubscript)
          <> primes
  return
    . annotate (AnnKind c)
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

ppPathSymbol :: PP PathSymbol
ppPathSymbol = \case
  PathP -> return (annotate (AnnKind KNameAxiom) "p")

ppIndexAt :: PP IndexAt
ppIndexAt IndexAt {..} = do
  b' <- ppTerm _indexAtBase
  ix' <- ppPathSymbol _indexAtPath
  return $
    b' <+> ppCodeAnn kwExclamation <+> ix'

ppStack :: Doc CodeAnn
ppStack = annotate (AnnKind KNameLocal) "S"

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

ppRule :: PP Rule
ppRule Rule {..} = do
  let sep_ r1 r2 = r1 <+> ppCodeAnn kwNockmaLogicAnd <+> r2
  conds' <- concatWith sep_ <$> mapM ppEvalRelation _ruleConditions
  let n = Text.length (toPlainText conds')
      hrule = pretty (Text.replicate (max n 3) "-")
  post' <- ppEvalRelation _rulePost
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
            <> r2
      )
      rules'
