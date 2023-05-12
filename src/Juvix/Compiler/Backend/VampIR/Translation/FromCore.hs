module Juvix.Compiler.Backend.VampIR.Translation.FromCore where

import Data.Text qualified as T
import Juvix.Compiler.Backend.VampIR.Language as VampIR
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (getInfoName)
import Juvix.Compiler.Core.Language as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNodeNames')

fromCore :: InfoTable -> Program
fromCore tab = fromCoreNode $ lookupIdentifierNode tab (fromJust (tab ^. infoMain))

fromCoreNode :: Node -> Program
fromCoreNode node =
  let (lams, body) = unfoldLambdas (disambiguateNodeNames' disambiguate emptyInfoTable node)
      (defs, expr) = convertLets body
   in Program
        { _programFunctions =
            [ Function
                { _functionName = "main",
                  _functionArguments = map (^. lambdaLhsBinder . binderName) lams,
                  _functionLocalDefs = defs,
                  _functionExpression = expr
                }
            ]
        }
  where
    isValidChar :: Char -> Bool
    isValidChar c = c == '_' || ((isLetter c || isDigit c) && isAscii c)

    mkName :: Text -> Text
    mkName ident = "var_" <> T.filter isValidChar ident

    disambiguate :: BinderList Binder -> Text -> Text
    disambiguate bl name = mkName name <> "_" <> show (length bl)

    convertLets :: Core.Node -> ([LocalDef], Expression)
    convertLets = \case
      NLet Let {..} ->
        let (defs, expr) = convertLets _letBody
            def =
              LocalDef
                { _localDefName = _letItem ^. letItemBinder . binderName,
                  _localDefValue = convertExpr (_letItem ^. letItemValue)
                }
         in (def : defs, expr)
      nd ->
        ([], convertExpr nd)

    convertExpr :: Core.Node -> Expression
    convertExpr = \case
      NVar Core.Var {..} -> ExpressionVar $ VampIR.Var (getInfoName _varInfo)
      NCst Constant {..} -> case _constantValue of
        ConstInteger i -> ExpressionConstant i
        _ -> impossible
      NBlt BuiltinApp {..} -> case _builtinAppArgs of
        [l, r] ->
          ExpressionBinop (Binop op (convertExpr l) (convertExpr r))
          where
            op = case _builtinAppOp of
              OpIntAdd -> OpAdd
              OpIntSub -> OpSub
              OpIntMul -> OpMul
              OpIntDiv -> OpDiv
              OpIntMod -> OpMod
              OpIntLt -> OpLt
              OpIntLe -> OpLe
              Core.OpEq -> VampIR.OpEq
              _ -> impossible
        _ -> case _builtinAppOp of
          OpFail -> ExpressionFail
          _ -> impossible
      NCtr Constr {..} -> case _constrTag of
        BuiltinTag TagTrue -> ExpressionConstant 1
        BuiltinTag TagFalse -> ExpressionConstant 0
        _ -> impossible
      NCase c -> translateCase translateIf impossible c
        where
          translateIf :: Node -> Node -> Node -> Expression
          translateIf val br1 br2 = ExpressionIfThenElse $ IfThenElse (convertExpr val) (convertExpr br1) (convertExpr br2)
      _ -> impossible
