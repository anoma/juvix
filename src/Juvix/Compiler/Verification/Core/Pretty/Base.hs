module Juvix.Compiler.Verification.Core.Pretty.Base where

import Juvix.Compiler.Verification.Core.Language
import Juvix.Compiler.Verification.Core.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

instance PrettyCode BinaryOp where
  ppCode BinaryOpAdd = do
    pure "BinaryOp.add_int"
  ppCode BinaryOpSub = do
    pure "BinaryOp.sub_int"
  ppCode BinaryOpMul = do
    pure "BinaryOp.mul_int"
  ppCode BinaryOpDiv = do
    pure "BinaryOp.div_int"

instance PrettyCode Var where
  ppCode (Var x) = do
    pure $ "Expr.var" <+> pretty x

instance PrettyCode Constant where
  ppCode = \case
    ConstantInteger x ->
      pure $ "Expr.const (Constant.int " <> pretty x <> ")"
    ConstantString x ->
      pure $ "Expr.const (Constant.string \"" <> pretty x <> "\")"

instance PrettyCode App where
  ppCode (App l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    pure $ "Expr.app" <+> l' <+> r'

instance PrettyCode ConstrApp where
  ppCode (ConstrApp l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    pure $ "Expr.constr_app" <+> l' <+> r'

instance PrettyCode Binop where
  ppCode (Binop o a1 a2) = do
    o' <- ppCode o
    a1' <- ppRightExpression appFixity a1
    a2' <- ppRightExpression appFixity a2
    pure $ "Expr.binop" <+> o' <+> a1' <+> a2'

instance PrettyCode Lambda where
  ppCode (Lambda b) = do
    b' <- ppRightExpression appFixity b
    pure $ "Expr.lambda" <+> b'

instance PrettyCode Save where
  ppCode (Save v b) = do
    v' <- ppRightExpression appFixity v
    b' <- ppRightExpression appFixity b
    pure $ "Expr.save" <+> v' <+> b'

instance PrettyCode Branch where
  ppCode (Branch c b n) = do
    b' <- ppRightExpression appFixity b
    n' <- ppRightExpression appFixity n
    pure $ "Expr.branch" <+> pretty c <+> b' <+> n'

instance PrettyCode Recur where
  ppCode (Recur b) = do
    b' <- ppRightExpression appFixity b
    pure $ "Expr.recur" <+> b'

instance PrettyCode Expr where
  ppCode = \case
    ExprVar v -> ppCode v
    ExprUnit -> pure "Expr.unit"
    ExprConst c -> ppCode c
    ExprConstr n -> pure $ "Expr.constr" <+> pretty n
    ExprApp a -> ppCode a
    ExprConstrApp c -> ppCode c
    ExprBinop b -> ppCode b
    ExprLambda l -> ppCode l
    ExprSave s -> ppCode s
    ExprBranch b -> ppCode b
    ExprRecur r -> ppCode r
    ExprFail -> pure "Expr.fail"

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

ppSequence ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  [a] ->
  Sem r (Doc Ann)
ppSequence vs = hsep <$> mapM (ppRightExpression appFixity) vs

docSequence :: (PrettyCode a, HasAtomicity a) => Options -> [a] -> Doc Ann
docSequence opts =
  run
    . runReader opts
    . ppSequence

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
