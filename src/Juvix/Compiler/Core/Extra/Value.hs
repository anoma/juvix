module Juvix.Compiler.Core.Extra.Value where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.ExpansionInfo (kExpansionInfo)
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Value

toValue :: InfoTable -> Node -> Value
toValue tab = \case
  NCst Constant {..} -> ValueConstant _constantValue
  NCtr c -> goConstr c
  NLam lam -> goLambda lam
  Closure {..} -> toValue tab (substEnv _closureEnv _closureNode)
  NPi {} -> goType
  NUniv {} -> goType
  NTyp {} -> goType
  NPrim {} -> goType
  NVar {} -> impossible
  NIdt {} -> impossible
  NApp {} -> impossible
  NBlt {} -> impossible
  NLet {} -> impossible
  NRec {} -> impossible
  NCase {} -> impossible
  NMatch {} -> impossible
  NDyn {} -> impossible
  NBot {} -> impossible
  where
    goConstr :: Constr -> Value
    goConstr Constr {..} =
      ValueConstrApp
        ConstrApp
          { _constrAppName = ci ^. constructorName,
            _constrAppFixity = ci ^. constructorFixity,
            _constrAppArgs = map (toValue tab) (drop paramsNum _constrArgs)
          }
      where
        ci = lookupConstructorInfo tab _constrTag
        ii = lookupInductiveInfo tab (ci ^. constructorInductive)
        paramsNum = length (ii ^. inductiveParams)

    goType :: Value
    goType = ValueType

    goLambda :: Lambda -> Value
    goLambda lam =
      let (lams, body) = unfoldLambdas (NLam lam)
          n = length $ takeWhile (Info.member kExpansionInfo . (^. lambdaLhsInfo)) lams
       in if
              | n < length lams ->
                  ValueFun
              | otherwise ->
                  case body of
                    NCtr c ->
                      toValue tab (NCtr (over constrArgs (dropEnd n) c))
                    _ -> ValueFun
