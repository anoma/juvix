module Juvix.Compiler.Verification.Core.Translation.FromCore
  ( fromCore,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList as BL
import Juvix.Compiler.Core.Data.IdentDependencyInfo qualified as Core
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Verification.Core.Extra
import Juvix.Compiler.Verification.Core.Language

data BinderInfo = BinderInfo
  { _binderInfoLevel :: Level,
    _binderInfoProjectionIndex :: Maybe Int
  }

makeLenses ''BinderInfo

letrecTupleConstr :: Text
letrecTupleConstr = "$LetRecTuple$"

fromCore :: Core.InfoTable -> Expr
fromCore infoTable = go mempty 0 sccs
  where
    mainNode = Core.lookupTabIdentifierNode infoTable (infoTable ^. Core.infoMain . to fromJust)
    sccs = buildSCCs (Core.createCallGraph infoTable)

    go :: HashMap Symbol BinderInfo -> Int -> [SCC Symbol] -> Expr
    go idents bindersNum = \case
      [] ->
        goNode infoTable idents mempty bindersNum mainNode
      (AcyclicSCC sym : xs) ->
        mkSave val (go idents' (bindersNum + 1) xs)
        where
          val = goNode infoTable idents mempty bindersNum (Core.lookupTabIdentifierNode infoTable sym)
          idents' = HashMap.insert sym (BinderInfo bindersNum Nothing) idents
      (CyclicSCC syms : xs) ->
        mkSave (mkRecur (mkConstrApps letrecTupleConstr values)) (go idents' bindersNum' xs)
        where
          values = map (goNode infoTable idents' mempty bindersNum' . Core.lookupTabIdentifierNode infoTable) syms
          idents0 = zipWithExact (\sym idx -> (sym, BinderInfo bindersNum (Just idx))) syms [0 .. length syms - 1]
          idents' = foldr (uncurry HashMap.insert) idents idents0
          bindersNum' = bindersNum + 1

-- `bindersNum` is the number of binders above for the result; `bl` is the list
-- of binders above the input node
goNode :: Core.InfoTable -> HashMap Symbol BinderInfo -> BinderList BinderInfo -> Int -> Core.Node -> Expr
goNode infoTable idents bl bindersNum node = case node of
  Core.NVar x -> goVar x
  Core.NIdt x -> goIdent x
  Core.NCst x -> goConstant x
  Core.NApp x -> goApp x
  Core.NBlt x -> goBuiltinApp x
  Core.NCtr x -> goConstr x
  Core.NLam x -> goLambda x
  Core.NLet x -> goLet x
  Core.NRec x -> goLetRec x
  Core.NCase x -> goCase x
  Core.NPi x -> goPi x
  Core.NUniv x -> goUniv x
  Core.NTyp x -> goTypeConstr x
  Core.NPrim x -> goTypePrim x
  Core.NDyn x -> goDynamic x
  Core.NBot x -> goBottom x
  Core.NMatch {} -> impossible
  Core.Closure {} -> impossible
  where
    unsupported :: a
    unsupported = error ("Unsupported node: " <> Core.ppTrace node)

    goNode' :: Core.Node -> Expr
    goNode' = goNode infoTable idents bl bindersNum

    goNodeUnderBinder :: Core.Node -> Expr
    goNodeUnderBinder = goNode infoTable idents bl' bindersNum'
      where
        bindersNum' = bindersNum + 1
        bl' = BL.cons (BinderInfo bindersNum Nothing) bl

    goTag :: Core.Tag -> Name
    goTag tag = Core.lookupTabConstructorInfo infoTable tag ^. Core.constructorName

    goBinder :: BinderInfo -> Expr
    goBinder bi =
      case bi ^. binderInfoProjectionIndex of
        Nothing ->
          var
        Just proj ->
          mkCase var [(letrecTupleConstr, mkVar proj)] Nothing
      where
        var = mkVar (bindersNum - bi ^. binderInfoLevel - 1)

    goVar :: Core.Var -> Expr
    goVar Core.Var {..} =
      goBinder (BL.lookup _varIndex bl)

    goIdent :: Core.Ident -> Expr
    goIdent Core.Ident {..} =
      goBinder $ fromJust $ HashMap.lookup _identSymbol idents

    goConstant :: Core.Constant -> Expr
    goConstant Core.Constant {..} = case _constantValue of
      Core.ConstInteger x -> mkInt x
      _ -> unsupported

    goApp :: Core.App -> Expr
    goApp Core.App {..} =
      mkApp (goNode' _appLeft) (goNode' _appRight)

    goBuiltinApp :: Core.BuiltinApp -> Expr
    goBuiltinApp Core.BuiltinApp {..} = case _builtinAppOp of
      Core.OpIntAdd -> mkBinop BinaryOpAdd
      Core.OpIntSub -> mkBinop BinaryOpSub
      Core.OpIntMul -> mkBinop BinaryOpMul
      Core.OpIntDiv -> mkBinop BinaryOpDiv
      _ -> unsupported
      where
        mkBinop :: BinaryOp -> Expr
        mkBinop op = case _builtinAppArgs of
          [arg1, arg2] ->
            ExprBinop
              Binop
                { _binopOper = op,
                  _binopArg1 = goNode' arg1,
                  _binopArg2 = goNode' arg2
                }
          _ -> unsupported

    goConstr :: Core.Constr -> Expr
    goConstr Core.Constr {..} =
      mkConstrApps (goTag _constrTag) (map goNode' _constrArgs)

    goLambda :: Core.Lambda -> Expr
    goLambda Core.Lambda {..} = mkLambda (goNodeUnderBinder _lambdaBody)

    goLet :: Core.Let -> Expr
    goLet Core.Let {..} = mkSave value body
      where
        value = goNode' (_letItem ^. Core.letItemValue)
        body = goNodeUnderBinder _letBody

    goLetRec :: Core.LetRec -> Expr
    goLetRec Core.LetRec {..} =
      mkSave (mkRecur (mkConstrApps letrecTupleConstr values)) body
      where
        values = map (goRec . (^. Core.letItemValue)) (toList _letRecValues)
        body = goRec _letRecBody
        bl' = BL.prepend (map (BinderInfo bindersNum . Just) [0 .. length _letRecValues - 1]) bl
        goRec = goNode infoTable idents bl' (bindersNum + 1)

    goCase :: Core.Case -> Expr
    goCase Core.Case {..} =
      mkSave
        (goNode' _caseValue)
        (foldr goCaseBranch defaultBranch _caseBranches)
      where
        defaultBranch = maybe ExprFail (goNode infoTable idents bl (bindersNum + 1)) _caseDefault

        goCaseBranch :: Core.CaseBranch -> Expr -> Expr
        goCaseBranch Core.CaseBranch {..} next =
          mkBranch (goTag _caseBranchTag) (goNode infoTable idents bl' bindersNum' _caseBranchBody) next
          where
            bindersNum' = bindersNum + length _caseBranchBinders
            bl' = BL.prependRev binders bl
            binders = map (\i -> BinderInfo i Nothing) [bindersNum .. bindersNum' - 1]

    goPi :: Core.Pi -> Expr
    goPi _ = ExprUnit

    goUniv :: Core.Univ -> Expr
    goUniv _ = ExprUnit

    goTypeConstr :: Core.TypeConstr -> Expr
    goTypeConstr _ = ExprUnit

    goTypePrim :: Core.TypePrim -> Expr
    goTypePrim _ = ExprUnit

    goDynamic :: Core.DynamicTy -> Expr
    goDynamic _ = ExprUnit

    goBottom :: Core.Bottom -> Expr
    goBottom _ = ExprFail
