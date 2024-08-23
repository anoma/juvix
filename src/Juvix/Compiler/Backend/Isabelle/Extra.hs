module Juvix.Compiler.Backend.Isabelle.Extra where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Backend.Isabelle.Language

mkApp :: Expression -> [Expression] -> Expression
mkApp fn = \case
  [] -> fn
  arg : args -> mkApp (ExprApp (Application fn arg)) args

-- | Check if a pattern `pat1` subsumes another pattern `pat2`.
subsumesPattern :: Pattern -> Pattern -> Bool
subsumesPattern pat1 pat2 = case (pat1, pat2) of
  (PatVar _, _) -> True
  (PatZero, PatZero) -> True
  (PatConstrApp (ConstrApp c1 p1), PatConstrApp (ConstrApp c2 p2)) ->
    c1 == c2 && all (uncurry subsumesPattern) (zipExact p1 p2)
  (PatTuple (Tuple p1), PatTuple (Tuple p2)) ->
    length p1 == length p2
      && all (uncurry subsumesPattern) (NonEmpty.zip p1 p2)
  (PatList (List p1), PatList (List p2)) ->
    length p1 == length p2
      && all (uncurry subsumesPattern) (zipExact p1 p2)
  (PatCons (Cons c1 p1), PatCons (Cons c2 p2)) ->
    subsumesPattern c1 c2 && subsumesPattern p1 p2
  (PatRecord (Record n1 r1), PatRecord (Record n2 r2)) ->
    n1 == n2
      && map ((^. nameText) . fst) r1' == map ((^. nameText) . fst) r2'
      && all (uncurry subsumesPattern) (zipExact (map snd r1') (map snd r2'))
    where
      r1' = sortOn ((^. nameText) . fst) r1
      r2' = sortOn ((^. nameText) . fst) r2
  _ -> False

-- | Rename all occurrences of a variable in an expression. Also renames bound
-- variables.
substVar :: Name -> Name -> Expression -> Expression
substVar var var' = go
  where
    go :: Expression -> Expression
    go = \case
      ExprIden x -> goIden x
      ExprUndefined -> ExprUndefined
      ExprLiteral x -> ExprLiteral x
      ExprApp x -> goApplication x
      ExprBinop x -> goBinop x
      ExprTuple x -> goTuple x
      ExprList x -> goList x
      ExprCons x -> goCons x
      ExprRecord x -> goRecord x
      ExprRecordUpdate x -> goRecordUpdate x
      ExprLet x -> goLet x
      ExprIf x -> goIf x
      ExprCase x -> goCase x
      ExprLambda x -> goLambda x

    goName :: Name -> Name
    goName x = if x ^. nameText == var ^. nameText then var' else x

    goIden :: Name -> Expression
    goIden = ExprIden . goName

    goApplication :: Application -> Expression
    goApplication (Application fn arg) =
      ExprApp (Application (go fn) (go arg))

    goBinop :: Binop -> Expression
    goBinop (Binop {..}) =
      ExprBinop
        Binop
          { _binopOperator,
            _binopLeft = go _binopLeft,
            _binopRight = go _binopRight,
            _binopFixity
          }

    goTuple :: Tuple Expression -> Expression
    goTuple (Tuple xs) = ExprTuple (Tuple (fmap go xs))

    goList :: List Expression -> Expression
    goList (List xs) = ExprList (List (map go xs))

    goCons :: Cons Expression -> Expression
    goCons (Cons h t) = ExprCons (Cons (go h) (go t))

    goRecord' :: Record Expression -> Record Expression
    goRecord' (Record n r) = Record n (map (second go) r)

    goRecord :: Record Expression -> Expression
    goRecord = ExprRecord . goRecord'

    goRecordUpdate :: RecordUpdate -> Expression
    goRecordUpdate (RecordUpdate r f) =
      ExprRecordUpdate (RecordUpdate (go r) (goRecord' f))

    goLet :: Let -> Expression
    goLet (Let cs e) = ExprLet (Let (fmap goLetClause cs) (go e))

    goLetClause :: LetClause -> LetClause
    goLetClause (LetClause n e) = LetClause (goName n) (go e)

    goIf :: If -> Expression
    goIf (If v t f) = ExprIf (If (go v) (go t) (go f))

    goCase :: Case -> Expression
    goCase (Case v bs) = ExprCase (Case (go v) (fmap goCaseBranch bs))

    goCaseBranch :: CaseBranch -> CaseBranch
    goCaseBranch (CaseBranch p e) = CaseBranch (goPattern p) (go e)

    goPattern :: Pattern -> Pattern
    goPattern = \case
      PatVar x -> PatVar (goName x)
      PatZero -> PatZero
      PatConstrApp (ConstrApp c p) -> PatConstrApp (ConstrApp c (fmap goPattern p))
      PatTuple (Tuple p) -> PatTuple (Tuple (fmap goPattern p))
      PatList (List p) -> PatList (List (fmap goPattern p))
      PatCons (Cons h t) -> PatCons (Cons (goPattern h) (goPattern t))
      PatRecord (Record n r) -> PatRecord (Record n (map (second goPattern) r))

    goLambda :: Lambda -> Expression
    goLambda (Lambda {..}) =
      ExprLambda
        Lambda
          { _lambdaVar = goName _lambdaVar,
            _lambdaBody = go _lambdaBody,
            _lambdaType
          }
