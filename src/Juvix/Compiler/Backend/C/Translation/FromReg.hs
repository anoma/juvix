module Juvix.Compiler.Backend.C.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.C.Data.CBuilder
import Juvix.Compiler.Backend.C.Data.Types
import Juvix.Compiler.Backend.C.Extra.Serialization
import Juvix.Compiler.Backend.C.Language as C
import Juvix.Compiler.Backend.C.Translation.FromReg.Base
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Info qualified as Reg
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
      ExternalDecl
        $ Declaration
          { _declType =
              DeclArray
                Array
                  { _arrayType = DeclTypeDefType "constr_info_t",
                    _arraySize = fromIntegral (info ^. Reg.extraInfoConstrsNum)
                  },
            _declName = Just "juvix_constr_info_array",
            _declInitializer =
              Just
                $ ListInitializer
                $ ExprInitializer (macroVar "BUILTIN_UIDS_INFO")
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
      ExternalDecl
        $ Declaration
          { _declType =
              DeclArray
                Array
                  { _arrayType = DeclTypeDefType "function_info_t",
                    _arraySize = fromIntegral (info ^. Reg.extraInfoFunctionsNum)
                  },
            _declName = Just "juvix_function_info_array",
            _declInitializer =
              Just
                $ ListInitializer
                $ map
                  (\ci -> ListInitializer [ExprInitializer $ string (ci ^. Reg.functionName)])
                  (HashMap.elems (tab ^. Reg.infoFunctions))
          }

    mainFunction :: CCode
    mainFunction =
      ExternalFunc
        $ Function
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
        ++ [ StatementExpr
               $ macroCall
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
      run
        $ runCBuilder
        $ concatMapM (fromRegFunction info) (HashMap.elems (tab ^. Reg.infoFunctions))

fromRegFunction :: (Member CBuilder r) => Reg.ExtraInfo -> Reg.FunctionInfo -> Sem r [Statement]
fromRegFunction info funInfo = do
  body <- fromRegCode bNoStack info (funInfo ^. Reg.functionCode)
  let tmpDecls = mkDecls "DECL_TMP" localVarsNum
  return
    [closureDecl, functionDecl, StatementCompound (tmpDecls ++ body)]
  where
    mkDecls :: Text -> Int -> [Statement]
    mkDecls decl n = map (\i -> StatementExpr (macroCall decl [integer i])) [0 .. n - 1]

    sym :: Reg.Symbol
    sym = funInfo ^. Reg.functionSymbol

    maxStackHeight :: Int
    maxStackHeight = getMaxStackHeight info sym

    localVarsNum :: Int
    localVarsNum = getLocalVarsNum info sym

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
              StatementCompound
                $ map
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
  Reg.Unop x ->
    return [fromUnaryOp x]
  Reg.Cairo {} ->
    unsupported "Cairo builtin"
  Reg.Assign Reg.InstrAssign {..} ->
    return $ stmtsAssign (fromVarRef _instrAssignResult) (fromValue _instrAssignValue)
  Reg.Assert Reg.InstrAssert {..} ->
    return [StatementExpr $ macroCall "JUVIX_ASSERT" [fromValue _instrAssertValue]]
  Reg.Trace Reg.InstrTrace {..} ->
    return [StatementExpr $ macroCall "JUVIX_TRACE" [fromValue _instrTraceValue]]
  Reg.Dump ->
    return [StatementExpr $ macroVar "JUVIX_DUMP"]
  Reg.Failure Reg.InstrFailure {..} ->
    return [StatementExpr $ macroCall "JUVIX_FAILURE" [fromValue _instrFailureValue]]
  Reg.Prealloc x ->
    return [fromPrealloc x]
  Reg.Alloc x ->
    return $ fromAlloc x
  Reg.AllocClosure x ->
    return $ fromAllocClosure x
  Reg.ExtendClosure x ->
    return $ fromExtendClosure x
  Reg.TailCall x ->
    return $ fromTailCall x
  Reg.Call x ->
    fromCall x
  Reg.TailCallClosures x ->
    return $ fromTailCallClosures x
  Reg.CallClosures x ->
    fromCallClosures x
  Reg.Return x ->
    return $ fromReturn x
  Reg.If x ->
    fromIf x
  Reg.Branch x ->
    fromBranch x
  Reg.Case x ->
    fromCase x
  Reg.Block Reg.InstrBlock {..} ->
    fromRegCode bNoStack info _instrBlockCode
  where
    unsupported :: Text -> a
    unsupported x = error ("unsupported: " <> x)

    fromBinaryOp :: Reg.InstrBinop -> Statement
    fromBinaryOp Reg.InstrBinop {..} =
      StatementExpr
        $ macroCall
          (getBinaryOpMacro _instrBinopOpcode)
          [ fromVarRef _instrBinopResult,
            fromValue _instrBinopArg1,
            fromValue _instrBinopArg2
          ]

    getBoolOpMacro :: Reg.BoolOp -> Text
    getBoolOpMacro = \case
      Reg.OpIntLt -> "JUVIX_BOOL_INT_LT"
      Reg.OpIntLe -> "JUVIX_BOOL_INT_LE"
      Reg.OpEq -> "JUVIX_BOOL_VAL_EQ"

    getBinaryOpMacro :: Reg.BinaryOp -> Text
    getBinaryOpMacro = \case
      Reg.OpIntAdd -> "JUVIX_INT_ADD"
      Reg.OpIntSub -> "JUVIX_INT_SUB"
      Reg.OpIntMul -> "JUVIX_INT_MUL"
      Reg.OpIntDiv -> "JUVIX_INT_DIV"
      Reg.OpIntMod -> "JUVIX_INT_MOD"
      Reg.OpBool Reg.OpIntLt -> "JUVIX_INT_LT"
      Reg.OpBool Reg.OpIntLe -> "JUVIX_INT_LE"
      Reg.OpBool Reg.OpEq -> "JUVIX_VAL_EQ"
      Reg.OpStrConcat -> "JUVIX_STR_CONCAT"
      Reg.OpFieldAdd -> unsupported "field type"
      Reg.OpFieldSub -> unsupported "field type"
      Reg.OpFieldMul -> unsupported "field type"
      Reg.OpFieldDiv -> unsupported "field type"

    fromUnaryOp :: Reg.InstrUnop -> Statement
    fromUnaryOp Reg.InstrUnop {..} =
      StatementExpr
        $ macroCall
          (getUnaryOpMacro _instrUnopOpcode)
          [ fromVarRef _instrUnopResult,
            fromValue _instrUnopArg
          ]

    getUnaryOpMacro :: Reg.UnaryOp -> Text
    getUnaryOpMacro = \case
      Reg.OpShow -> "JUVIX_SHOW"
      Reg.OpStrToInt -> "JUVIX_STR_TO_INT"
      Reg.OpArgsNum -> "JUVIX_ARGS_NUM"
      Reg.OpFieldToInt -> unsupported "field type"
      Reg.OpIntToField -> unsupported "field type"
      Reg.OpUInt8ToInt -> "JUVIX_UINT8_TO_INT"
      Reg.OpIntToUInt8 -> "JUVIX_INT_TO_UINT8"

    fromVarRef :: Reg.VarRef -> Expression
    fromVarRef Reg.VarRef {..} =
      macroCall g [ExpressionLiteral (LiteralInt (toInteger _varRefIndex))]
      where
        g =
          case _varRefGroup of
            Reg.VarGroupArgs -> "ARG"
            Reg.VarGroupLocal -> "TMP"

    fromValue :: Reg.Value -> Expression
    fromValue = \case
      Reg.ValConst c -> fromConst c
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

    fromConst :: Reg.Constant -> Expression
    fromConst = \case
      Reg.ConstInt x -> macroCall "make_smallint" [integer x]
      Reg.ConstField {} -> impossible
      Reg.ConstBool True -> macroVar "BOOL_TRUE"
      Reg.ConstBool False -> macroVar "BOOL_FALSE"
      Reg.ConstString x -> macroCall "GET_CONST_CSTRING" [integer (getStringId info x)]
      Reg.ConstUnit -> macroVar "OBJ_UNIT"
      Reg.ConstVoid -> macroVar "OBJ_VOID"
      Reg.ConstUInt8 x -> macroCall "make_smallint" [integer x]
      Reg.ConstByteArray {} -> impossible

    fromPrealloc :: Reg.InstrPrealloc -> Statement
    fromPrealloc Reg.InstrPrealloc {..} =
      StatementExpr
        $ macroCall
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
          stmtsAssign (fromVarRef _instrAllocResult) (fromValue (head' _instrAllocArgs))

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
              ExpressionStatement
                $ StatementCompound
                $ stmtsAssignArgs
                  (Just (ExpressionVar "juvix_closure_nargs"))
                  "CLOSURE_ARG"
                  (Just (ExpressionVar "juvix_temp_var"))
                  _instrExtendClosureArgs
            ]
        )
        : stmtsAssign (fromVarRef _instrExtendClosureResult) (ExpressionVar "juvix_temp_var")

    fromTailCall :: Reg.InstrTailCall -> [Statement]
    fromTailCall Reg.InstrTailCall {..} =
      case _instrTailCallType of
        Reg.CallFun sym ->
          stmtsAssignFunArgs _instrTailCallArgs
            ++ [ StatementExpr
                   $ macroCall
                     (if bNoStack then "TAIL_CALL_NS" else "TAIL_CALL")
                     [integer (getFUID info sym), exprLabel info sym]
               ]
        Reg.CallClosure vr ->
          stmtsAssignCArgs vr _instrTailCallArgs
            ++ [ StatementExpr
                   $ macroCall
                     (if bNoStack then "TAIL_CALL_CLOSURE_NS" else "TAIL_CALL_CLOSURE")
                     [fromVarRef vr]
               ]

    fromCall :: Reg.InstrCall -> Sem r [Statement]
    fromCall Reg.InstrCall {..} = do
      lab <- freshLabel
      return
        $ case _instrCallType of
          Reg.CallFun sym ->
            stmtsPushVars _instrCallLiveVars
              ++ stmtsAssignFunArgs _instrCallArgs
              ++ [ StatementExpr
                     $ macroCall
                       "CALL"
                       [integer (getFUID info sym), exprLabel info sym, ExpressionVar lab],
                   stmtAssign (fromVarRef _instrCallResult) (ExpressionVar "juvix_result")
                 ]
              ++ stmtsPopVars _instrCallLiveVars
          Reg.CallClosure vr ->
            stmtsPushVars _instrCallLiveVars
              ++ stmtsAssignCArgs vr _instrCallArgs
              ++ [ StatementExpr
                     $ macroCall
                       "CALL_CLOSURE"
                       [fromVarRef vr, ExpressionVar lab],
                   stmtAssign (fromVarRef _instrCallResult) (ExpressionVar "juvix_result")
                 ]
              ++ stmtsPopVars _instrCallLiveVars

    fromTailCallClosures :: Reg.InstrTailCallClosures -> [Statement]
    fromTailCallClosures Reg.InstrTailCallClosures {..}
      | argsNum <= info ^. Reg.extraInfoSpecialisedApply =
          stmtsAssignCArgs _instrTailCallClosuresValue _instrTailCallClosuresArgs
            ++ [ StatementExpr
                   $ macroCall
                     ("TAIL_APPLY_" <> show argsNum)
                     [fromVarRef _instrTailCallClosuresValue]
               ]
      | otherwise =
          stmtsAssignCArgs _instrTailCallClosuresValue _instrTailCallClosuresArgs
            ++ [ StatementExpr
                   $ macroCall
                     "TAIL_APPLY"
                     [ fromVarRef _instrTailCallClosuresValue,
                       integer argsNum
                     ]
               ]
      where
        argsNum = length _instrTailCallClosuresArgs

    fromCallClosures :: Reg.InstrCallClosures -> Sem r [Statement]
    fromCallClosures Reg.InstrCallClosures {..} = do
      lab <- freshLabel
      return
        $ stmtsPushVars _instrCallClosuresLiveVars
        ++ stmtsAssignCArgs _instrCallClosuresValue _instrCallClosuresArgs
        ++ [ call lab,
             stmtAssign (fromVarRef _instrCallClosuresResult) (ExpressionVar "juvix_result")
           ]
        ++ stmtsPopVars _instrCallClosuresLiveVars
      where
        argsNum = length _instrCallClosuresArgs
        call lab =
          if
            | argsNum <= info ^. Reg.extraInfoSpecialisedApply ->
                StatementExpr
                  $ macroCall
                    ("APPLY_" <> show argsNum)
                    [ fromVarRef _instrCallClosuresValue,
                      ExpressionVar lab
                    ]
            | otherwise ->
                StatementExpr
                  $ macroCall
                    "APPLY"
                    [ fromVarRef _instrCallClosuresValue,
                      integer argsNum,
                      ExpressionVar lab
                    ]
    fromIf :: Reg.InstrIf -> Sem r [Statement]
    fromIf Reg.InstrIf {..} = do
      br1 <- fromRegCode bNoStack info _instrIfTrue
      br2 <- fromRegCode bNoStack info _instrIfFalse
      return
        [ StatementIf
            $ If
              { _ifCondition =
                  macroCall
                    "is_true"
                    [ macroCall
                        (getBoolOpMacro _instrIfOp)
                        [ fromValue _instrIfArg1,
                          fromValue _instrIfArg2
                        ]
                    ],
                _ifThen = StatementCompound br1,
                _ifElse = Just (StatementCompound br2)
              }
        ]

    fromBranch :: Reg.InstrBranch -> Sem r [Statement]
    fromBranch Reg.InstrBranch {..} = do
      br1 <- fromRegCode bNoStack info _instrBranchTrue
      br2 <- fromRegCode bNoStack info _instrBranchFalse
      return
        [ StatementIf
            $ If
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
            [ StatementSwitch
                $ Switch
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
      [ StatementExpr
          $ macroCall
            "ASSIGN_CARGS"
            [ fromVarRef vr,
              ExpressionStatement
                $ StatementCompound
                $ stmtsAssignArgs (Just (ExpressionVar "juvix_closure_nargs")) "CARG" Nothing args
            ]
      ]

    stmtsPushVars :: [Reg.VarRef] -> [Statement]
    stmtsPushVars =
      stmtsPush . map fromVarRef

    stmtsPopVars :: [Reg.VarRef] -> [Statement]
    stmtsPopVars =
      stmtsPop . map fromVarRef
