module Juvix.Data.Ape.Pretty
  ( module Juvix.Data.Ape.Base,
    ApeParams (..),
    runApe,
    ppApe,
  )
where

import Juvix.Data.Ape.Base
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Pretty as PP

newtype ApeParams a = ApeParams
  { _apePP :: a -> Doc CodeAnn
  }

makeLenses ''ApeParams

runApe :: forall a e. (IsApe a e) => ApeParams e -> a -> Doc CodeAnn
runApe p a = run . runReader p . ppApe $ ape
  where
    ape :: Ape e
    ape = toApe a

ppLeaf :: (Members '[Reader (ApeParams a)] r) => Leaf a -> Sem r (Doc CodeAnn)
ppLeaf l = do
  pp <- asks (^. apePP)
  return (pp (l ^. leafExpr))

ppApe :: (Members '[Reader (ApeParams a)] r) => Ape a -> Sem r (Doc CodeAnn)
ppApe = ppCape . toCape

ppCape :: (Members '[Reader (ApeParams a)] r) => Cape a -> Sem r (Doc CodeAnn)
ppCape = \case
  CapeLeaf l -> ppLeaf l
  CapeChain c -> ppChain c
  CapeAppChain c -> ppAppChain c
  CapeUChain c -> ppUChain c

chain :: Doc CodeAnn -> NonEmpty (Doc CodeAnn) -> Doc CodeAnn
chain f' args' = PP.group (nest' (vsep (f' : toList args')))

ppAppChain :: forall a r. (Members '[Reader (ApeParams a)] r) => AppChain a -> Sem r (Doc CodeAnn)
ppAppChain (AppChain f links) = do
  f' <- ppLinkExpr fx f
  args' <- mapM (ppLinkExpr fx) links
  return $ chain f' args'
  where
    fx :: Precedence
    fx = appFixity ^. fixityPrecedence

ppChain :: forall a r. (Members '[Reader (ApeParams a)] r) => Chain a -> Sem r (Doc CodeAnn)
ppChain (Chain opFix f links) = do
  f' <- ppLinkExpr fx f
  args' <- mapM ppLink links
  return $ chain f' args'
  where
    fx :: Precedence
    fx = opFix ^. fixityPrecedence
    ppLink :: (a, Cape a) -> Sem r (Doc CodeAnn)
    ppLink (op, a) = do
      pp <- asks (^. apePP)
      let op' = pp op
      a' <- ppLinkExpr fx a
      return (op' <+> a')

ppUChain :: forall a r. (Members '[Reader (ApeParams a)] r) => UChain a -> Sem r (Doc CodeAnn)
ppUChain (UChain opFix f links) = do
  f' <- ppLinkExpr fx f
  pp <- asks (^. apePP)
  let args = hsep (fmap pp links)
  return $ f' <+> args
  where
    fx :: Precedence
    fx = opFix ^. fixityPrecedence

ppLinkExpr ::
  (Members '[Reader (ApeParams a)] r) => Precedence -> Cape a -> Sem r (Doc CodeAnn)
ppLinkExpr opFix e = parensCond cond <$> ppCape e
  where
    cond = apeParens (atomicity e) opFix

apeParens :: Atomicity -> Precedence -> Bool
apeParens argAtom opPrec = case argAtom of
  Atom -> False
  -- if the precedences are equal, since they are not part of the same chain it
  -- means that they do not associate and thus parens are needed.
  Aggregate argFix -> argFix ^. fixityPrecedence <= opPrec
