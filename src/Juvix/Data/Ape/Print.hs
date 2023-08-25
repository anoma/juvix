module Juvix.Data.Ape.Print
  ( module Juvix.Data.Ape.Base,
    ApeParams (..),
    runApe,
    ppApe,
  )
where

import Juvix.Data.Ape.Base
import Juvix.Data.Effect.ExactPrint
import Juvix.Prelude hiding ((<+>), (<+?>), (<?+>), (?<>))

newtype ApeParams a = ApeParams
  { _apePP :: forall r. Members '[ExactPrint] r => a -> Sem r ()
  }

makeLenses ''ApeParams

runApe :: forall a e r. (IsApe a e, Members '[ExactPrint] r) => ApeParams e -> a -> Sem r ()
runApe p = runReader p . ppApe . toApe @a @e

ppLeaf :: Members '[Reader (ApeParams a), ExactPrint] r => Leaf a -> Sem r ()
ppLeaf l = do
  pp <- asks (^. apePP)
  pp (l ^. leafExpr)

ppApe :: Members '[Reader (ApeParams a), ExactPrint] r => Ape a -> Sem r ()
ppApe = ppCape . toCape

ppCape :: (Members '[Reader (ApeParams a), ExactPrint] r) => Cape a -> Sem r ()
ppCape = \case
  CapeLeaf l -> ppLeaf l
  CapeChain c -> ppChain c
  CapeAppChain c -> ppAppChain c
  CapeUChain c -> ppUChain c

chain :: Members '[ExactPrint] r => Sem r () -> Sem r ()
chain = grouped . nest

ppAppChain :: forall a r. (Members '[Reader (ApeParams a), ExactPrint] r) => AppChain a -> Sem r ()
ppAppChain (AppChain f links) = do
  let f' = ppLinkExpr fx f
      args' = ppLinkExpr fx <$> links
  chain (vsep (f' : toList args'))
  where
    fx :: Precedence
    fx = appFixity ^. fixityPrecedence

ppChain :: forall a r. (Members '[Reader (ApeParams a), ExactPrint] r) => Chain a -> Sem r ()
ppChain (Chain opFix f links) = do
  let f' = ppLinkExpr fx f
  chain (ppLinks f' (toList links))
  where
    ppLinks :: Sem r () -> [Link a] -> Sem r ()
    ppLinks acc = \case
      [] -> acc
      l : ls -> do
        let sepHelper a b = a <> sp <> b
            sp
              | l ^. linkIsDelimiter = lineOrEmpty
              | otherwise = line
        pp <- asks (^. apePP)
        let op' = pp (l ^. linkOp)
            a' = ppLinkExpr fx (l ^. linkArg)
        ppLinks (acc `sepHelper` op' <+> a') ls

    fx :: Precedence
    fx = opFix ^. fixityPrecedence

ppUChain :: forall a r. (Members '[Reader (ApeParams a), ExactPrint] r) => UChain a -> Sem r ()
ppUChain (UChain opFix f links) = do
  pp <- asks (^. apePP)
  let f' = ppLinkExpr fx f
      args = hsep (fmap pp links)
  f' <> sp <> args
  where
    fx :: Precedence
    fx = opFix ^. fixityPrecedence

    sp :: Sem r ()
    sp = case fx of
      PrecUpdate -> return ()
      _ -> space

ppLinkExpr ::
  (Members '[Reader (ApeParams a), ExactPrint] r) => Precedence -> Cape a -> Sem r ()
ppLinkExpr opFix e = parensIf cond (ppCape e)
  where
    cond = apeParens (atomicity e) opFix

apeParens :: Atomicity -> Precedence -> Bool
apeParens argAtom opPrec = case argAtom of
  Atom -> False
  -- if the precedences are equal, since they are not part of the same chain it
  -- means that they do not associate and thus parens are needed.
  Aggregate argFix -> argFix ^. fixityPrecedence <= opPrec
