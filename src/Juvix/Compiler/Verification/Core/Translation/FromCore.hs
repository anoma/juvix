module Juvix.Compiler.Verification.Core.Translation.FromCore
  ( fromCore,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Verification.Core.Extra
import Juvix.Compiler.Verification.Core.Language

data IdentInfo = IdentInfo
  { _identInfoLevel :: Level,
    _identInfoProjectionIndex :: Maybe Int
  }

makeLenses ''IdentInfo

fromCore :: Core.InfoTable -> Expr
fromCore = todo

goNode :: Core.InfoTable -> HashMap Symbol IdentInfo -> BinderList Level -> Int -> Core.Node -> Expr
goNode infoTable idents bl bindersNum = \case
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
    letrecTupleConstr :: Text
    letrecTupleConstr = "$LetRecTuple$"

    goVar :: Core.Var -> Expr
    goVar Core.Var {..} = mkVar _varIndex

    goIdent :: Core.Ident -> Expr
    goIdent Core.Ident {..} =
      case identInfo ^. identInfoProjectionIndex of
        Nothing ->
          var
        Just proj ->
          mkCase var [(letrecTupleConstr, mkVar proj)] Nothing
      where
        identInfo = fromJust $ HashMap.lookup _identSymbol idents
        var = mkVar (bindersNum - identInfo ^. identInfoLevel - 1)

    goConstant :: Core.Constant -> Expr
    goConstant = todo

    goApp :: Core.App -> Expr
    goApp = todo

    goBuiltinApp :: Core.BuiltinApp -> Expr
    goBuiltinApp = todo

    goConstr :: Core.Constr -> Expr
    goConstr = todo

    goLambda :: Core.Lambda -> Expr
    goLambda = todo

    goLet :: Core.Let -> Expr
    goLet = todo

    goLetRec :: Core.LetRec -> Expr
    goLetRec = todo

    goCase :: Core.Case -> Expr
    goCase = todo

    goPi :: Core.Pi -> Expr
    goPi = todo

    goUniv :: Core.Univ -> Expr
    goUniv = todo

    goTypeConstr :: Core.TypeConstr -> Expr
    goTypeConstr = todo

    goTypePrim :: Core.TypePrim -> Expr
    goTypePrim = todo

    goDynamic :: Core.DynamicTy -> Expr
    goDynamic = todo

    goBottom :: Core.Bottom -> Expr
    goBottom = todo
