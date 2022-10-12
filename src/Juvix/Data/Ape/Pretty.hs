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

runApe :: forall a e. IsApe a e => ApeParams e -> a -> Doc CodeAnn
runApe p a = run . runReader p . ppApe $ ape
  where
    ape :: Ape e
    ape = toApe a

ppLeaf :: Members '[Reader (ApeParams a)] r => Leaf a -> Sem r (Doc CodeAnn)
ppLeaf l = do
  pp <- asks (^. apePP)
  return (pp (l ^. leafExpr))

ppApe :: Members '[Reader (ApeParams a)] r => Ape a -> Sem r (Doc CodeAnn)
ppApe = ppCape . toCape

ppCape :: Members '[Reader (ApeParams a)] r => Cape a -> Sem r (Doc CodeAnn)
ppCape = \case
  CapeLeaf l -> ppLeaf l
  CapeChain c -> ppChain c
  CapeUChain {} -> error "todo"

ppChain :: forall a r. Members '[Reader (ApeParams a)] r => Chain a -> Sem r (Doc CodeAnn)
ppChain (Chain fx f links) = do
  f' <- ppLinkExpr fx f
  args' <- mapM ppLink links
  return $ PP.group (f' <> nest 2 (line <> vsep args'))
  where
    ppLink :: (Maybe a, Cape a) -> Sem r (Doc CodeAnn)
    ppLink (op, a) = do
      pp <- asks (^. apePP)
      let op' = pp <$> op
      a' <- ppLinkExpr fx a
      return (op' <?+> a')

ppLinkExpr ::
  Members '[Reader (ApeParams a)] r => Fixity -> Cape a -> Sem r (Doc CodeAnn)
ppLinkExpr fixlr e =
  parensCond (apeParens (atomicity e) fixlr) <$> ppCape e

apeParens :: Atomicity -> Fixity -> Bool
apeParens argAtom opInf = case argAtom of
  Atom -> False
  Aggregate argInf -> argInf <= opInf
