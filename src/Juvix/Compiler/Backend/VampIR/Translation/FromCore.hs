module Juvix.Compiler.Backend.VampIR.Translation.FromCore where

import Data.Text qualified as T
import Juvix.Compiler.Backend.VampIR.Extra (getVampIRInputs)
import Juvix.Compiler.Backend.VampIR.Language as VampIR
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (getInfoName)
import Juvix.Compiler.Core.Language as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNodeNames')

fromCore :: InfoTable -> Program
fromCore tab = fromCoreNode ii node
  where
    sym = fromJust (tab ^. infoMain)
    node = lookupIdentifierNode tab sym
    ii = lookupIdentifierInfo tab sym

fromCoreNode :: IdentifierInfo -> Node -> Program
fromCoreNode ii node =
  let (lams, body) = unfoldLambdas (disambiguateNodeNames' disambiguate emptyInfoTable node)
      (defs, expr) = convertLets body
      n = length lams
      args = getVampIRInputs n (ii ^. identifierArgNames)
      isBoolTarget = isTypeBool (typeTarget (ii ^. identifierType))
   in Program
        { _programFunctions =
            [ Function
                { _functionName = "main",
                  _functionArguments = map (^. lambdaLhsBinder . binderName) lams,
                  _functionLocalDefs = defs,
                  _functionExpression = expr,
                  _functionInputs = args,
                  _functionOutput = if isBoolTarget then "1" else "out"
                }
            ]
        }
  where
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
      NVar x -> goVar x
      NCst x -> goConstant x
      NBlt x -> goBuiltinApp x
      NCtr x -> goConstructor x
      NCase c -> goCase c
      _ -> impossible

    goVar :: Core.Var -> Expression
    goVar Core.Var {..} = ExpressionVar $ VampIR.Var (getInfoName _varInfo)

    goConstant :: Constant -> Expression
    goConstant Constant {..} = case _constantValue of
      ConstInteger i -> ExpressionConstant i
      _ -> impossible

    goBuiltinApp :: BuiltinApp -> Expression
    goBuiltinApp BuiltinApp {..} = case _builtinAppArgs of
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

    goConstructor :: Constr -> Expression
    goConstructor Constr {..} = case _constrTag of
      BuiltinTag TagTrue -> ExpressionConstant 1
      BuiltinTag TagFalse -> ExpressionConstant 0
      _ -> impossible

    goCase :: Case -> Expression
    goCase c = translateCase translateIf impossible c
      where
        translateIf :: Node -> Node -> Node -> Expression
        translateIf val br1 br2 = ExpressionIfThenElse $ IfThenElse (convertExpr val) (convertExpr br1) (convertExpr br2)
