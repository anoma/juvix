module Juvix.Compiler.Backend.C.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.C.Data.CBuilder
import Juvix.Compiler.Backend.C.Data.Types
import Juvix.Compiler.Backend.C.Extra.Serialization
import Juvix.Compiler.Backend.C.Language as C
import Juvix.Compiler.Backend.C.Translation.FromReg.Base
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Data.Fixity qualified as Fixity
import Juvix.Prelude

fromReg :: Limits -> Reg.InfoTable -> MiniCResult
fromReg lims tab =
  MiniCResult $ serialize $ CCodeUnit [includeApi, constrInfo, functionInfo, mainFunction]
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo lims tab

    includeApi :: CCode
    includeApi = ExternalMacro (CppIncludeSystem "juvix/api.h")

    constrInfo :: CCode
    constrInfo =
      ExternalDecl $
        Declaration
          { _declType =
              DeclArray
                Array
                  { _arrayType = DeclTypeDefType "constr_info_t",
                    _arraySize = fromIntegral (info ^. Reg.extraInfoConstrsNum)
                  },
            _declName = Just "juvix_constr_info_array",
            _declInitializer =
              Just $
                ListInitializer $
                  ExprInitializer (macroVar "BUILTIN_UIDS_INFO")
                    : map
                      ( \ci ->
                          ListInitializer
                            [ ExprInitializer $ string (ci ^. Reg.constructorName),
                              ExprInitializer $ constrIsBinop (ci ^. Reg.constructorFixity),
                              constrFixity (ci ^. Reg.constructorFixity)
                            ]
                      )
                      (Reg.userConstrs tab)
          }
      where
        constrIsBinop :: Maybe Fixity -> Expression
        constrIsBinop = \case
          Just fixity -> integer (fromEnum (isBinary fixity))
          Nothing -> integer (0 :: Int)

        constrFixity :: Maybe Fixity -> Initializer
        constrFixity = \case
          Just fixity ->
            ListInitializer
              [ ExprInitializer $ getPrec (fixity ^. fixityPrecedence),
                ExprInitializer $ ExpressionVar $ getAssoc (fixity ^. fixityArity)
              ]
          Nothing -> ExprInitializer $ macroVar "APP_FIXITY"

        getPrec :: Precedence -> Expression
        getPrec = \case
          PrecUpdate -> macroVar "PREC_UPDATE"
          PrecApp -> macroVar "PREC_APP"
          PrecArrow -> macroVar "PREC_ARROW"
          PrecNat n -> integer n

        getAssoc :: OperatorArity -> Text
        getAssoc = \case
          Fixity.OpUnary _ -> "assoc_none"
          Fixity.OpBinary AssocNone -> "assoc_none"
          Fixity.OpBinary AssocLeft -> "assoc_left"
          Fixity.OpBinary AssocRight -> "assoc_right"
          Fixity.OpNone -> "assoc_none"

    functionInfo :: CCode
    functionInfo =
      ExternalDecl $
        Declaration
          { _declType =
              DeclArray
                Array
                  { _arrayType = DeclTypeDefType "function_info_t",
                    _arraySize = fromIntegral (info ^. Reg.extraInfoFunctionsNum)
                  },
            _declName = Just "juvix_function_info_array",
            _declInitializer =
              Just $
                ListInitializer $
                  map
                    (\ci -> ListInitializer [ExprInitializer $ string (ci ^. Reg.functionName)])
                    (HashMap.elems (tab ^. Reg.infoFunctions))
          }

    mainFunction :: CCode
    mainFunction =
      ExternalFunc $
        Function
          { _funcSig = mainSig,
            _funcBody = map BodyStatement mainBody
          }

    mainSig :: FunctionSig
    mainSig =
      FunctionSig
        { _funcReturnType = DeclTypeDefType "int",
          _funcQualifier = None,
          _funcName = "main",
          _funcArgs = []
        }

    mainBody :: [Statement]
    mainBody =
      argDecls
        ++ [ StatementExpr $
               macroCall
                 "JUVIX_PROLOGUE"
                 [integer (info ^. Reg.extraInfoMaxArgsNum + info ^. Reg.extraInfoMaxCallClosuresArgsNum)]
           ]
        ++ makeCStrings
        ++ [ stmtAssign (ExpressionVar "juvix_constrs_num") (integer (info ^. Reg.extraInfoConstrsNum)),
             stmtAssign (ExpressionVar "juvix_constr_info") (ExpressionVar "juvix_constr_info_array"),
             stmtAssign (ExpressionVar "juvix_functions_num") (integer (info ^. Reg.extraInfoFunctionsNum)),
             stmtAssign (ExpressionVar "juvix_function_info") (ExpressionVar "juvix_function_info_array")
           ]
        ++ jumpMainFunction
        ++ juvixFunctions
        ++ [ StatementExpr $ macroVar "JUVIX_EPILOGUE",
             StatementReturn (Just (integer (0 :: Int)))
           ]

    argDecls :: [Statement]
    argDecls =
      map (\(i :: Int) -> StatementExpr $ macroCall "DECL_REG_ARG" [integer i]) [0 .. min 3 (n - 1)]
        ++ map (\i -> StatementExpr $ macroCall "DECL_ARG" [integer i]) [4 .. n - 1]
      where
        n = info ^. Reg.extraInfoMaxArgsNum

    makeCStrings :: [Statement]
    makeCStrings =
      if
          | n > 0 ->
              StatementExpr (macroCall "DECL_CONST_CSTRINGS" [integer n])
                : StatementExpr (macroVar "SAVE_MEMORY_POINTERS")
                : map
                  ( \(txt, i) ->
                      StatementExpr $ macroCall "MAKE_CONST_CSTRING" [integer i, string txt]
                  )
                  (HashMap.toList (info ^. Reg.extraInfoStringMap))
                ++ [StatementExpr (macroVar "RESTORE_MEMORY_POINTERS")]
          | otherwise -> []
      where
        n = HashMap.size (info ^. Reg.extraInfoStringMap)

    jumpMainFunction :: [Statement]
    jumpMainFunction = case tab ^. Reg.infoMainFunction of
      Nothing -> error "no main function"
      Just main -> [StatementGoto $ Goto $ getLabel info main]

    juvixFunctions :: [Statement]
    juvixFunctions =
      run $
        runCBuilder $
          concatMapM (fromRegFunction info) (HashMap.elems (tab ^. Reg.infoFunctions))

fromRegFunction :: (Member CBuilder r) => Reg.ExtraInfo -> Reg.FunctionInfo -> Sem r [Statement]
fromRegFunction info funInfo = do
  body <- fromRegCode bNoStack info (funInfo ^. Reg.functionCode)
  let stmpDecls = mkDecls "DECL_STMP" (funInfo ^. Reg.functionStackVarsNum)
      tmpDecls = mkDecls "DECL_TMP" (funInfo ^. Reg.functionTempVarsNum)
  return
    [closureDecl, functionDecl, StatementCompound (stmpDecls ++ tmpDecls ++ body)]
  where
    mkDecls :: Text -> Int -> [Statement]
    mkDecls decl n = map (\i -> StatementExpr (macroCall decl [integer i])) [0 .. n - 1]

    sym :: Reg.Symbol
    sym = funInfo ^. Reg.functionSymbol

    maxStackHeight :: Int
    maxStackHeight = getMaxStackHeight info sym

    bNoStack :: Bool
    bNoStack = maxStackHeight == 0

    functionDecl :: Statement
    functionDecl =
      if
          | bNoStack ->
              StatementExpr
                (macroCall "JUVIX_FUNCTION_NS" [exprLabel info sym])
          | otherwise ->
              StatementExpr
                (macroCall "JUVIX_FUNCTION" [exprLabel info sym, integer maxStackHeight])

    closureDecl :: Statement
    closureDecl =
      StatementLabel
        Label
          { _labelName = getClosureLabel info sym,
            _labelCode =
              StatementCompound $
                map
                  (\i -> stmtAssign (macroCall "ARG" [integer i]) (macroCall "CARG" [integer i]))
                  [0 .. funInfo ^. Reg.functionArgsNum - 1]
          }

fromRegCode :: (Member CBuilder r) => Bool -> Reg.ExtraInfo -> Reg.Code -> Sem r [Statement]
fromRegCode bNoStack info = concatMapM (fromRegInstr bNoStack info)

fromRegInstr :: forall r. (Member CBuilder r) => Bool -> Reg.ExtraInfo -> Reg.Instruction -> Sem r [Statement]
fromRegInstr bNoStack info = \case
  Reg.Nop ->
    return []
  Reg.Binop x ->
    return [fromBinaryOp x]
  Reg.Show Reg.InstrShow {..} ->
    return [StatementExpr $ macroCall "JUVIX_SHOW" [fromVarRef _instrShowResult, fromValue _instrShowValue]]
  Reg.StrToInt Reg.InstrStrToInt {..} ->
    return [StatementExpr $ macroCall "JUVIX_STR_TO_INT" [fromVarRef _instrStrToIntResult, fromValue _instrStrToIntValue]]
  Reg.Assign Reg.InstrAssign {..} ->
    return $ stmtsAssign (fromVarRef _instrAssignResult) (fromValue _instrAssignValue)
  Reg.Trace Reg.InstrTrace {..} ->
    return [StatementExpr $ macroCall "JUVIX_TRACE" [fromValue _instrTraceValue]]
  Reg.Dump ->
    return [StatementExpr $ macroVar "JUVIX_DUMP"]
  Reg.Failure Reg.InstrFailure {..} ->
    return [StatementExpr $ macroCall "JUVIX_FAILURE" [fromValue _instrFailureValue]]
  Reg.ArgsNum Reg.InstrArgsNum {..} ->
    return [StatementExpr $ macroCall "JUVIX_ARGS_NUM" [fromVarRef _instrArgsNumResult, fromValue _instrArgsNumValue]]
  Reg.Prealloc x ->
    return [fromPrealloc x]
  Reg.Alloc x ->
    return $ fromAlloc x
  Reg.AllocClosure x ->
    return $ fromAllocClosure x
  Reg.ExtendClosure x ->
    return $ fromExtendClosure x
  Reg.Call x@Reg.InstrCall {..}
    | _instrCallIsTail ->
        return $ fromTailCall x
  Reg.Call x ->
    fromCall x
  Reg.CallClosures x@Reg.InstrCallClosures {..}
    | _instrCallClosuresIsTail ->
        return $ fromTailCallClosures x
  Reg.CallClosures x ->
    fromCallClosures x
  Reg.Return x ->
    return $ fromReturn x
  Reg.Branch x ->
    fromBranch x
  Reg.Case x ->
    fromCase x
  Reg.Block Reg.InstrBlock {..} ->
    fromRegCode bNoStack info _instrBlockCode
  where
    fromBinaryOp :: Reg.BinaryOp -> Statement
    fromBinaryOp Reg.BinaryOp {..} =
      StatementExpr $
        macroCall
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
      Reg.OpStrConcat -> "JUVIX_STR_CONCAT"

    fromVarRef :: Reg.VarRef -> Expression
    fromVarRef Reg.VarRef {..} =
      macroCall g [ExpressionLiteral (LiteralInt (toInteger _varRefIndex))]
      where
        g =
          case _varRefGroup of
            Reg.VarGroupArgs -> "ARG"
            Reg.VarGroupStack -> "STMP"
            Reg.VarGroupTemp -> "TMP"

    fromValue :: Reg.Value -> Expression
    fromValue = \case
      Reg.ConstInt x -> macroCall "make_smallint" [integer x]
      Reg.ConstBool True -> macroVar "BOOL_TRUE"
      Reg.ConstBool False -> macroVar "BOOL_FALSE"
      Reg.ConstString x -> macroCall "GET_CONST_CSTRING" [integer (getStringId info x)]
      Reg.ConstUnit -> macroVar "OBJ_UNIT"
      Reg.ConstVoid -> macroVar "OBJ_VOID"
      Reg.CRef Reg.ConstrField {..} ->
        case _constrFieldMemRep of
          Reg.MemRepConstr ->
            macroCall "CONSTR_ARG" [fromVarRef _constrFieldRef, integer _constrFieldIndex]
          Reg.MemRepTag ->
            macroCall "MAKE_HEADER" [integer (getUID info _constrFieldTag), integer (0 :: Int)]
          Reg.MemRepTuple ->
            macroCall "FIELD" [fromVarRef _constrFieldRef, integer _constrFieldIndex]
          Reg.MemRepUnit ->
            macroVar "OBJ_UNIT"
          Reg.MemRepUnpacked {} ->
            fromVarRef _constrFieldRef
      Reg.VRef x -> fromVarRef x

    fromPrealloc :: Reg.InstrPrealloc -> Statement
    fromPrealloc Reg.InstrPrealloc {..} =
      StatementExpr $
        macroCall
          "PREALLOC"
          [ integer _instrPreallocWordsNum,
            ExpressionStatement $ StatementCompound $ stmtsPushVars _instrPreallocLiveVars,
            ExpressionStatement $ StatementCompound $ stmtsPopVars _instrPreallocLiveVars
          ]

    fromAlloc :: Reg.InstrAlloc -> [Statement]
    fromAlloc a@Reg.InstrAlloc {..} =
      case _instrAllocMemRep of
        Reg.MemRepConstr ->
          if
              | null _instrAllocArgs ->
                  stmtsCall
                    "ALLOC_CONSTR_BOXED_TAG"
                    [fromVarRef _instrAllocResult, integer $ getUID info _instrAllocTag]
              | otherwise ->
                  stmtsAllocConstr a "ALLOC_CONSTR_BOXED" "CONSTR_ARG"
        Reg.MemRepTag ->
          stmtsCall
            "ALLOC_CONSTR_UNBOXED"
            [fromVarRef _instrAllocResult, integer $ getUID info _instrAllocTag]
        Reg.MemRepTuple ->
          stmtsAllocConstr a "ALLOC_CONSTR_TUPLE" "FIELD"
        Reg.MemRepUnit ->
          stmtsAssign (fromVarRef _instrAllocResult) (macroVar "OBJ_UNIT")
        Reg.MemRepUnpacked {} ->
          stmtsAssign (fromVarRef _instrAllocResult) (fromValue (List.head _instrAllocArgs))

    fromAllocClosure :: Reg.InstrAllocClosure -> [Statement]
    fromAllocClosure Reg.InstrAllocClosure {..} =
      StatementExpr
        ( macroCall
            "ALLOC_CLOSURE"
            [ ExpressionVar "juvix_temp_var",
              integer (getFUID info _instrAllocClosureSymbol),
              exprAddr info _instrAllocClosureSymbol,
              integer (length _instrAllocClosureArgs),
              integer (_instrAllocClosureExpectedArgsNum - length _instrAllocClosureArgs)
            ]
        )
        : stmtsAssignArgs
          Nothing
          "CLOSURE_ARG"
          (Just (ExpressionVar "juvix_temp_var"))
          _instrAllocClosureArgs
        ++ stmtsAssign (fromVarRef _instrAllocClosureResult) (ExpressionVar "juvix_temp_var")

    fromExtendClosure :: Reg.InstrExtendClosure -> [Statement]
    fromExtendClosure Reg.InstrExtendClosure {..} =
      StatementExpr
        ( macroCall
            "EXTEND_CLOSURE"
            [ ExpressionVar "juvix_temp_var",
              fromVarRef _instrExtendClosureValue,
              integer (length _instrExtendClosureArgs),
              ExpressionStatement $
                StatementCompound $
                  stmtsAssignArgs
                    (Just (ExpressionVar "juvix_closure_nargs"))
                    "CLOSURE_ARG"
                    (Just (ExpressionVar "juvix_temp_var"))
                    _instrExtendClosureArgs
            ]
        )
        : stmtsAssign (fromVarRef _instrExtendClosureResult) (ExpressionVar "juvix_temp_var")

    fromTailCall :: Reg.InstrCall -> [Statement]
    fromTailCall Reg.InstrCall {..} =
      case _instrCallType of
        Reg.CallFun sym ->
          stmtsAssignFunArgs _instrCallArgs
            ++ [ StatementExpr $
                   macroCall
                     (if bNoStack then "TAIL_CALL_NS" else "TAIL_CALL")
                     [integer (getFUID info sym), exprLabel info sym]
               ]
        Reg.CallClosure vr ->
          stmtsAssignCArgs vr _instrCallArgs
            ++ [ StatementExpr $
                   macroCall
                     (if bNoStack then "TAIL_CALL_CLOSURE_NS" else "TAIL_CALL_CLOSURE")
                     [fromVarRef vr]
               ]

    fromCall :: Reg.InstrCall -> Sem r [Statement]
    fromCall Reg.InstrCall {..} = do
      lab <- freshLabel
      return $
        case _instrCallType of
          Reg.CallFun sym ->
            stmtsPushVars _instrCallLiveVars
              ++ stmtsAssignFunArgs _instrCallArgs
              ++ [ StatementExpr $
                     macroCall
                       "CALL"
                       [integer (getFUID info sym), exprLabel info sym, ExpressionVar lab],
                   stmtAssign (fromVarRef _instrCallResult) (ExpressionVar "juvix_result")
                 ]
              ++ stmtsPopVars _instrCallLiveVars
          Reg.CallClosure vr ->
            stmtsPushVars _instrCallLiveVars
              ++ stmtsAssignCArgs vr _instrCallArgs
              ++ [ StatementExpr $
                     macroCall
                       "CALL_CLOSURE"
                       [fromVarRef vr, ExpressionVar lab],
                   stmtAssign (fromVarRef _instrCallResult) (ExpressionVar "juvix_result")
                 ]
              ++ stmtsPopVars _instrCallLiveVars

    fromTailCallClosures :: Reg.InstrCallClosures -> [Statement]
    fromTailCallClosures Reg.InstrCallClosures {..}
      | argsNum <= 3 =
          stmtsAssignCArgs _instrCallClosuresValue _instrCallClosuresArgs
            ++ [ StatementExpr $
                   macroCall
                     ("TAIL_APPLY_" <> show argsNum)
                     [fromVarRef _instrCallClosuresValue]
               ]
      | otherwise =
          stmtsAssignCArgs _instrCallClosuresValue _instrCallClosuresArgs
            ++ [ StatementExpr $
                   macroCall
                     "TAIL_APPLY"
                     [ fromVarRef _instrCallClosuresValue,
                       integer argsNum
                     ]
               ]
      where
        argsNum = length _instrCallClosuresArgs

    fromCallClosures :: Reg.InstrCallClosures -> Sem r [Statement]
    fromCallClosures Reg.InstrCallClosures {..} = do
      lab <- freshLabel
      return $
        stmtsPushVars _instrCallClosuresLiveVars
          ++ stmtsAssignCArgs _instrCallClosuresValue _instrCallClosuresArgs
          ++ [ call lab,
               stmtAssign (fromVarRef _instrCallClosuresResult) (ExpressionVar "juvix_result")
             ]
          ++ stmtsPopVars _instrCallClosuresLiveVars
      where
        argsNum = length _instrCallClosuresArgs
        call lab =
          if
              | argsNum <= 3 ->
                  StatementExpr $
                    macroCall
                      ("APPLY_" <> show argsNum)
                      [ fromVarRef _instrCallClosuresValue,
                        ExpressionVar lab
                      ]
              | otherwise ->
                  StatementExpr $
                    macroCall
                      "APPLY"
                      [ fromVarRef _instrCallClosuresValue,
                        integer argsNum,
                        ExpressionVar lab
                      ]

    fromBranch :: Reg.InstrBranch -> Sem r [Statement]
    fromBranch Reg.InstrBranch {..} = do
      br1 <- fromRegCode bNoStack info _instrBranchTrue
      br2 <- fromRegCode bNoStack info _instrBranchFalse
      return
        [ StatementIf $
            If
              { _ifCondition = macroCall "is_true" [fromValue _instrBranchValue],
                _ifThen = StatementCompound br1,
                _ifElse = Just (StatementCompound br2)
              }
        ]

    fromCase :: Reg.InstrCase -> Sem r [Statement]
    fromCase Reg.InstrCase {..} = do
      brs <- mapM fromCaseBranch _instrCaseBranches
      def <- case _instrCaseDefault of
        Nothing -> return Nothing
        Just code -> Just <$> fromRegCode bNoStack info code
      case _instrCaseIndRep of
        Reg.IndRepStandard ->
          return
            [ StatementSwitch $
                Switch
                  { _switchCondition = macroCall "get_header" [fromValue _instrCaseValue],
                    _switchCases = brs,
                    _switchDefault = fmap StatementCompound def
                  }
            ]
        Reg.IndRepEnum ->
          error "unsupported constructor representation"
        Reg.IndRepEnumRecord ->
          error "unsupported constructor representation"
        Reg.IndRepEnumMaybe {} ->
          error "unsupported constructor representation"
        Reg.IndRepRecord ->
          error "unsupported constructor representation"
        Reg.IndRepUnit ->
          error "unsupported constructor representation"
        Reg.IndRepNewtype {} ->
          error "unsupported constructor representation"
        Reg.IndRepMixed ->
          error "unsupported constructor representation"

    fromCaseBranch :: Reg.CaseBranch -> Sem r Case
    fromCaseBranch Reg.CaseBranch {..} = do
      stmts <- fromRegCode bNoStack info _caseBranchCode
      return
        Case
          { _caseValue =
              macroCall
                "MAKE_HEADER"
                [ integer (getUID info _caseBranchTag),
                  integer _caseBranchArgsNum
                ],
            _caseCode = StatementCompound stmts
          }

    fromReturn :: Reg.InstrReturn -> [Statement]
    fromReturn Reg.InstrReturn {..} =
      [ stmtAssign (ExpressionVar "juvix_result") (fromValue _instrReturnValue),
        StatementExpr (macroVar (if bNoStack then "RETURN_NS" else "RETURN"))
      ]

    stmtsAllocConstr :: Reg.InstrAlloc -> Text -> Text -> [Statement]
    stmtsAllocConstr Reg.InstrAlloc {..} alloc carg =
      StatementExpr
        ( macroCall
            alloc
            [ ExpressionVar "juvix_temp_var",
              integer $ getUID info _instrAllocTag,
              integer $ length _instrAllocArgs
            ]
        )
        : stmtsAssignArgs Nothing carg (Just (ExpressionVar "juvix_temp_var")) _instrAllocArgs
        ++ stmtsAssign (fromVarRef _instrAllocResult) (ExpressionVar "juvix_temp_var")

    stmtsAssignArgs :: Maybe Expression -> Text -> Maybe Expression -> [Reg.Value] -> [Statement]
    stmtsAssignArgs off carg ref args =
      zipWith
        ( \v idx ->
            stmtAssign (macroCall carg (maybeToList ref ++ [getIndex idx])) (fromValue v)
        )
        args
        [0 ..]
      where
        getIndex :: Int -> Expression
        getIndex idx =
          case off of
            Nothing -> integer idx
            Just e -> ExpressionBinary $ C.Binary C.Plus e (integer idx)

    stmtsAssignFunArgs :: [Reg.Value] -> [Statement]
    stmtsAssignFunArgs = stmtsAssignArgs Nothing "ARG" Nothing

    stmtsAssignCArgs :: Reg.VarRef -> [Reg.Value] -> [Statement]
    stmtsAssignCArgs vr args =
      [ StatementExpr $
          macroCall
            "ASSIGN_CARGS"
            [ fromVarRef vr,
              ExpressionStatement $
                StatementCompound $
                  stmtsAssignArgs (Just (ExpressionVar "juvix_closure_nargs")) "CARG" Nothing args
            ]
      ]

    stmtsPushVars :: [Reg.VarRef] -> [Statement]
    stmtsPushVars =
      stmtsPush . map fromVarRef

    stmtsPopVars :: [Reg.VarRef] -> [Statement]
    stmtsPopVars =
      stmtsPop . map fromVarRef
