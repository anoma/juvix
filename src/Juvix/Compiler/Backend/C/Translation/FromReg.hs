module Juvix.Compiler.Backend.C.Translation.FromReg where

import Data.List qualified as List
import Juvix.Compiler.Backend.C.Extra.Serialization qualified as C
import Juvix.Compiler.Backend.C.Language qualified as C
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Prelude

fromReg :: Reg.InfoTable -> Text
fromReg tab = undefined

fromRegFunction :: Reg.InfoTable -> Reg.FunctionInfo -> [C.BodyItem]
fromRegFunction tab funInfo = undefined

fromRegCode :: Reg.InfoTable -> Reg.Code -> [C.Statement]
fromRegCode tab code = undefined

fromRegInstr :: Reg.InfoTable -> Reg.Instruction -> [C.Statement]
fromRegInstr tab = \case
  Reg.Nop ->
    []
  Reg.Binop x ->
    [fromBinaryOp x]
  Reg.Assign Reg.InstrAssign {..} ->
    stmtsAssign (fromVarRef _instrAssignResult) (fromValue _instrAssignValue)
  Reg.Trace Reg.InstrTrace {..} ->
    [C.StatementExpr $ C.macroCall "JUVIX_TRACE" [fromValue _instrTraceValue]]
  Reg.Dump ->
    [C.StatementExpr $ C.macroVar "JUVIX_DUMP"]
  Reg.Failure Reg.InstrFailure {..} ->
    [C.StatementExpr $ C.macroCall "JUVIX_FAILURE" [fromValue _instrFailure]]
  Reg.Prealloc Reg.InstrPrealloc {..} ->
    [ C.StatementExpr $
        C.macroCall
          "PREALLOC"
          [ C.integer _instrPreallocWordsNum,
            C.ExpressionStatement $ C.StatementCompound $ stmtsPushVars _instrPreallocLiveVars,
            C.ExpressionStatement $ C.StatementCompound $ stmtsPopVars _instrPreallocLiveVars
          ]
    ]
  Reg.Alloc a@Reg.InstrAlloc {..} ->
    case _instrAllocMemRep of
      Reg.MemRepConstr ->
        stmtsAllocConstr a "ALLOC_CONSTR_BOXED" "CONSTR_ARG"
      Reg.MemRepTag ->
        [ C.StatementExpr $
            C.macroCall
              "ALLOC_CONSTR_UNBOXED"
              [fromVarRef _instrAllocResult, C.integer $ getUID _instrAllocTag]
        ]
      Reg.MemRepTuple ->
        stmtsAllocConstr a "ALLOC_CONSTR_TUPLE" "FIELD"
      Reg.MemRepUnit ->
        stmtsAssign (fromVarRef _instrAllocResult) (C.macroVar "OBJ_UNIT")
      Reg.MemRepUnpacked {} ->
        stmtsAssign (fromVarRef _instrAllocResult) (fromValue (List.head _instrAllocArgs))
  Reg.AllocClosure Reg.InstrAllocClosure {..} ->
    C.StatementExpr
      ( C.macroCall
          "ALLOC_CLOSURE"
          [ fromVarRef _instrAllocClosureResult,
            C.integer (getFUID _instrAllocClosureSymbol),
            exprAddr _instrAllocClosureSymbol,
            C.integer (length _instrAllocClosureArgs),
            C.integer (_instrAllocClosureExpectedArgsNum - length _instrAllocClosureArgs)
          ]
      )
      : stmtsAssignArgs Nothing "CLOSURE_ARG" (Just _instrAllocClosureResult) _instrAllocClosureArgs
  Reg.ExtendClosure Reg.InstrExtendClosure {..} ->
    [ C.StatementExpr $
        C.macroCall
          "EXTEND_CLOSURE"
          [ fromVarRef _instrExtendClosureResult,
            fromVarRef _instrExtendClosureValue,
            C.integer (length _instrExtendClosureArgs),
            C.ExpressionStatement $
              C.StatementCompound $
                stmtsAssignArgs
                  (Just (C.ExpressionVar "juvix_closure_nargs"))
                  "CLOSURE_ARG"
                  (Just _instrExtendClosureResult)
                  _instrExtendClosureArgs
          ]
    ]
  Reg.Call Reg.InstrCall {..} ->
    ( if
          | _instrCallIsTail -> []
          | otherwise ->
              stmtsPushVars _instrCallLiveVars
    )
      ++ stmtsAssignArgs
        Nothing
        "ARG"
        Nothing
        _instrCallArgs
      ++ ( if
               | _instrCallIsTail ->
                   [ C.StatementExpr $
                       C.macroCall
                         "CALL"
                         []
                   ]
               | otherwise -> []
         )
  Reg.CallClosures Reg.InstrCallClosures {..} ->
    []
  Reg.Return ->
    []
  Reg.Branch Reg.InstrBranch {..} ->
    []
  Reg.Case Reg.InstrCase {..} ->
    []
  where
    getUID :: Reg.Tag -> Int
    getUID = undefined

    getFUID :: Reg.Symbol -> Int
    getFUID = undefined

    getLabel :: Reg.Symbol -> Text
    getLabel = undefined

    getClosureLabel :: Reg.Symbol -> Text
    getClosureLabel = undefined

    fromBinaryOp :: Reg.BinaryOp -> C.Statement
    fromBinaryOp Reg.BinaryOp {..} =
      C.StatementExpr $
        C.macroCall
          (getOpcodeMacro _binaryOpCode)
          [ fromVarRef _binaryOpResult,
            fromValue _binaryOpArg1,
            fromValue _binaryOpArg2
          ]

    getOpcodeMacro :: Reg.Opcode -> Text
    getOpcodeMacro = \case
      Reg.OpIntAdd -> "JUVIX_INT_ADD"
      Reg.OpIntSub -> "JUVIX_INT_SUB"
      Reg.OpIntMul -> "JUVIX_INT_MUL"
      Reg.OpIntDiv -> "JUVIX_INT_DIV"
      Reg.OpIntMod -> "JUVIX_INT_MOD"
      Reg.OpIntLt -> "JUVIX_INT_LT"
      Reg.OpIntLe -> "JUVIX_INT_LE"
      Reg.OpEq -> "JUVIX_VAL_EQ"

    fromVarRef :: Reg.VarRef -> C.Expression
    fromVarRef Reg.VarRef {..} =
      C.macroCall g [C.ExpressionLiteral (C.LiteralInt (toInteger _varRefIndex))]
      where
        g =
          case _varRefGroup of
            Reg.VarGroupArgs -> "ARG"
            Reg.VarGroupStack -> "STMP"
            Reg.VarGroupTemp -> "TMP"

    fromValue :: Reg.Value -> C.Expression
    fromValue = \case
      Reg.ConstInt x -> C.macroCall "make_smallint" [C.integer x]
      Reg.ConstBool True -> C.macroVar "BOOL_TRUE"
      Reg.ConstBool False -> C.macroVar "BOOL_FALSE"
      Reg.ConstString x -> C.macroCall "MAKE_CSTRING" [C.ExpressionLiteral (C.LiteralString x)]
      Reg.ConstUnit -> C.macroVar "OBJ_UNIT"
      Reg.ConstVoid -> C.macroVar "OBJ_VOID"
      Reg.CRef Reg.ConstrField {..} ->
        case _constrFieldMemRep of
          Reg.MemRepConstr ->
            C.macroCall "CONSTR_ARG" [fromVarRef _constrFieldRef, C.integer _constrFieldIndex]
          Reg.MemRepTag ->
            C.macroCall "MAKE_HEADER" [C.integer (getUID _constrFieldTag), C.integer (0 :: Int)]
          Reg.MemRepTuple ->
            C.macroCall "FIELD" [fromVarRef _constrFieldRef, C.integer _constrFieldIndex]
          Reg.MemRepUnit ->
            C.macroVar "OBJ_UNIT"
          Reg.MemRepUnpacked {} ->
            fromVarRef _constrFieldRef
      Reg.VRef x -> fromVarRef x

    exprAddr :: Reg.Symbol -> C.Expression
    exprAddr sym = C.macroCall "LABEL_ADDR" [exprClosureLabel sym]

    exprLabel :: Reg.Symbol -> C.Expression
    exprLabel sym = C.ExpressionVar $ getLabel sym

    exprClosureLabel :: Reg.Symbol -> C.Expression
    exprClosureLabel sym = C.ExpressionVar $ getClosureLabel sym

    stmtsAllocConstr :: Reg.InstrAlloc -> Text -> Text -> [C.Statement]
    stmtsAllocConstr Reg.InstrAlloc {..} alloc carg =
      C.StatementExpr
        ( C.macroCall
            alloc
            [ fromVarRef _instrAllocResult,
              C.integer $ getUID _instrAllocTag,
              C.integer $ length _instrAllocArgs
            ]
        )
        : stmtsAssignArgs Nothing carg (Just _instrAllocResult) _instrAllocArgs

    stmtAssign :: C.Expression -> C.Expression -> C.Statement
    stmtAssign result value =
      C.StatementExpr $ C.ExpressionAssign (C.Assign result value)

    stmtsAssign :: C.Expression -> C.Expression -> [C.Statement]
    stmtsAssign result value = [stmtAssign result value]

    stmtsAssignArgs :: Maybe C.Expression -> Text -> Maybe Reg.VarRef -> [Reg.Value] -> [C.Statement]
    stmtsAssignArgs off carg ref args =
      zipWith
        ( \v idx ->
            stmtAssign (C.macroCall carg (maybe [] (\x -> [fromVarRef x]) ref ++ [getIndex idx])) (fromValue v)
        )
        args
        [0 ..]
      where
        getIndex :: Int -> C.Expression
        getIndex idx =
          case off of
            Nothing -> C.integer idx
            Just e -> C.ExpressionBinary $ C.Binary C.Plus e (C.integer idx)

    stmtsPushVars :: [Reg.VarRef] -> [C.Statement]
    stmtsPushVars =
      map (\vr -> C.StatementExpr $ C.macroCall "STACK_PUSH" [fromVarRef vr])

    stmtsPopVars :: [Reg.VarRef] -> [C.Statement]
    stmtsPopVars =
      map (\vr -> C.StatementExpr $ C.macroCall "STACK_POP" [fromVarRef vr])
        . reverse
