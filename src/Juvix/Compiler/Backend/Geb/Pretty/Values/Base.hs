module Juvix.Compiler.Backend.Geb.Pretty.Values.Base where

import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Pretty.Base qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Keywords
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude

doc :: (HasAtomicity c, PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      case atomicity x of
        Atom -> ppCode x
        Aggregate _ -> parens <$> ppCode x

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

ppCode' :: (PrettyCode c) => Options -> c -> Doc Ann
ppCode' opts = run . runReader opts . ppCode

instance PrettyCode ValueMorphismPair where
  ppCode ValueMorphismPair {..} = do
    left <- ppArg _valueMorphismPairLeft
    right <- ppArg _valueMorphismPairRight
    return $ kwPair <> line <> indent' (vsep [left, right])

instance PrettyCode ValueClosure where
  ppCode ValueClosure {..} = do
    lamb <- Geb.ppCode _valueClosureLambda
    return $ kwLamb <> line <> indent' lamb

instance PrettyCode GebValue where
  ppCode = \case
    GebValueMorphismUnit -> return kwUnit
    GebValueMorphismInteger n -> return $ annotate AnnLiteralInteger (pretty n)
    GebValueMorphismLeft val -> do
      v <- ppArg val
      return $ kwLeft <> line <> indent' v
    GebValueMorphismRight val -> do
      v <- ppArg val
      return $ kwRight <> line <> indent' v
    GebValueMorphismPair x -> ppCode x
    GebValueClosure x -> ppCode x

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

ppArg ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  a ->
  Sem r (Doc Ann)
ppArg = ppRightExpression appFixity

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e
