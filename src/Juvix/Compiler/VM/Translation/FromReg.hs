module Juvix.Compiler.VM.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend (defaultLimits)
import Juvix.Compiler.Backend.C.Translation.FromReg.Base
import Juvix.Compiler.Backend.VampIR.Extra (getVampIRInputs)
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Compiler.VM.Extra.Utils
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Translation.FromReg.Builder

fromReg :: Reg.InfoTable -> [Instruction]
fromReg tab = jumpMainFunction ++ fns
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo defaultLimits tab

    exitLabel :: Text
    exitLabel = "juvix_exit"

    jumpMainFunction :: [Instruction]
    jumpMainFunction = case tab ^. Reg.infoMainFunction of
      Nothing -> error "no main function"
      Just main ->
        mkArgAssigns (map VarRef args)
          ++ [ mkMove (LMemRef regSp) (LabelRef exitLabel),
               mkBinop OpIntAdd (LRegRef regSp) (RegRef regSp) (Const 1),
               mkJump (LabelRef (getLabel info main)),
               mkLabel exitLabel
             ]
        where
          mainInfo = fromJust $ HashMap.lookup main (tab ^. Reg.infoFunctions)
          args = getVampIRInputs (mainInfo ^. Reg.functionArgsNum) (mainInfo ^. Reg.functionArgNames)

    mkArgAssigns :: [Value] -> [Instruction]
    mkArgAssigns vals =
      zipWith (\val reg -> mkMove (LMemRef reg) val) vals [2 ..]

    fns :: [Instruction]
    fns = run $ runBuilder $ concatMapM (fromRegFunction info) (HashMap.elems (tab ^. Reg.infoFunctions))

fromRegFunction :: Member Builder r => Reg.ExtraInfo -> Reg.FunctionInfo -> Sem r [Instruction]
fromRegFunction info funInfo = do
  body <- fromRegCode info funInfo (funInfo ^. Reg.functionCode)
  return $ mkLabel (getLabel info (funInfo ^. Reg.functionSymbol)) : body

fromRegCode :: Member Builder r => Reg.ExtraInfo -> Reg.FunctionInfo -> Reg.Code -> Sem r [Instruction]
fromRegCode info funInfo = concatMapM (fromRegInstr info funInfo)

fromRegInstr :: forall r. Member Builder r => Reg.ExtraInfo -> Reg.FunctionInfo -> Reg.Instruction -> Sem r [Instruction]
fromRegInstr info funInfo = \case
  Reg.Nop ->
    return []
  Reg.Binop x ->
    return $ fromBinaryOp x
  Reg.Show {} ->
    unsupported
  Reg.StrToInt {} ->
    unsupported
  Reg.Assign Reg.InstrAssign {..} ->
    return $ instrs ++ [mkMove (LRegRef (fromVarRef _instrAssignResult)) val]
    where
      (instrs, val) = fromValue _instrAssignValue
  Reg.Trace {} ->
    unsupported
  Reg.Dump ->
    unsupported
  Reg.Failure {} ->
    return [mkMove (LRegRef 2) (Const 0), Halt]
  Reg.Prealloc {} ->
    return []
  Reg.Alloc x ->
    return $ fromAlloc x
  Reg.AllocClosure {} ->
    unsupported
  Reg.ExtendClosure {} ->
    unsupported
  Reg.Call x@Reg.InstrCall {..}
    | _instrCallIsTail ->
        return $ fromTailCall x
  Reg.Call x ->
    fromCall x
  Reg.CallClosures {} ->
    unsupported
  Reg.Return x ->
    return $ fromReturn x
  Reg.Branch x ->
    fromBranch x
  Reg.Case x ->
    fromCase x
  where
    unsupported :: a
    unsupported = error "unsupported"

    fromBinaryOp :: Reg.BinaryOp -> [Instruction]
    fromBinaryOp Reg.BinaryOp {..} =
      instrs1
        ++ mvs
        ++ instrs2
        ++ [ mkBinop
               (getOpcode _binaryOpCode)
               (LRegRef (fromVarRef _binaryOpResult))
               val1'
               val2
           ]
      where
        (instrs1, val1) = fromValue _binaryOpArg1
        (instrs2, val2) = fromValue _binaryOpArg2
        (mvs, val1') =
          if
              | null instrs1 || null instrs2 -> ([], val1)
              | otherwise -> ([mkMove (LRegRef (tmpReg + 1)) (RegRef tmpReg)], RegRef (tmpReg + 1))

    getOpcode :: Reg.Opcode -> Opcode
    getOpcode = \case
      Reg.OpIntAdd -> OpIntAdd
      Reg.OpIntSub -> OpIntSub
      Reg.OpIntMul -> OpIntMul
      Reg.OpIntDiv -> OpIntDiv
      Reg.OpIntMod -> OpIntMod
      Reg.OpIntLt -> OpIntLt
      Reg.OpIntLe -> unsupported
      Reg.OpEq -> OpIntEq
      Reg.OpStrConcat -> unsupported

    fromVarRef :: Reg.VarRef -> Int
    fromVarRef Reg.VarRef {..} =
      _varRefIndex + g + 2
      where
        g =
          case _varRefGroup of
            Reg.VarGroupArgs -> 0
            Reg.VarGroupStack -> funInfo ^. Reg.functionArgsNum
            Reg.VarGroupTemp -> funInfo ^. Reg.functionArgsNum + funInfo ^. Reg.functionStackVarsNum

    tmpReg :: Int
    tmpReg = funInfo ^. Reg.functionArgsNum + funInfo ^. Reg.functionStackVarsNum + funInfo ^. Reg.functionTempVarsNum + 2

    resultReg :: Int
    resultReg = 2

    mkAssigns :: Int -> [Reg.Value] -> [Instruction]
    mkAssigns reg =
      concatMap
        ( ( \(instrs, val) ->
              instrs ++ mkPush reg val
          )
            . fromValue
        )

    mkArgAssigns :: [Reg.Value] -> [Instruction]
    mkArgAssigns vals =
      concatMap
        ( ( \((instrs, val), reg) ->
              instrs ++ [mkMove (LMemRef reg) val]
          )
            . first fromValue
        )
        (zip vals [2 ..])

    mkPush :: Int -> Value -> [Instruction]
    mkPush reg val =
      [ mkMove (LMemRef reg) val,
        mkBinop OpIntAdd (LRegRef reg) (RegRef reg) (Const 1)
      ]

    mkPop :: Int -> LValue -> [Instruction]
    mkPop reg lval =
      [ mkBinop OpIntSub (LRegRef reg) (RegRef reg) (Const 1),
        mkMove lval (MemRef reg)
      ]

    mkPushVars :: [Reg.VarRef] -> [Instruction]
    mkPushVars = concatMap (mkPush regSp . RegRef . fromVarRef)

    mkPopVars :: [Reg.VarRef] -> [Instruction]
    mkPopVars = concatMap (mkPop regSp . LRegRef . fromVarRef) . reverse

    fromValue :: Reg.Value -> ([Instruction], Value)
    fromValue = \case
      Reg.ConstInt x -> ([], Const (fromInteger x))
      Reg.ConstBool True -> ([], Const 1)
      Reg.ConstBool False -> ([], Const 0)
      Reg.ConstString {} -> unsupported
      Reg.ConstUnit -> ([], Const 0)
      Reg.ConstVoid -> ([], Const 0)
      Reg.CRef Reg.ConstrField {..} ->
        case _constrFieldMemRep of
          Reg.MemRepConstr
            | _constrFieldIndex == 0 ->
                ([], MemRef (fromVarRef _constrFieldRef))
          Reg.MemRepConstr ->
            ( [ mkBinop OpIntAdd (LRegRef tmpReg) (RegRef (fromVarRef _constrFieldRef)) (Const _constrFieldIndex)
              ],
              MemRef tmpReg
            )
          Reg.MemRepTag ->
            ([], Const (getUID info _constrFieldTag))
          Reg.MemRepTuple ->
            unsupported
          Reg.MemRepUnit ->
            unsupported
          Reg.MemRepUnpacked {} ->
            unsupported
      Reg.VRef x -> ([], RegRef (fromVarRef x))

    fromAlloc :: Reg.InstrAlloc -> [Instruction]
    fromAlloc Reg.InstrAlloc {..} =
      case _instrAllocMemRep of
        Reg.MemRepConstr ->
          [ mkMove (LRegRef r) (RegRef regHp),
            mkMove (LMemRef regHp) (Const (getUID info _instrAllocTag)),
            mkBinop OpIntAdd (LRegRef regHp) (RegRef regHp) (Const 1)
          ]
            ++ mkAssigns regHp _instrAllocArgs
          where
            r = fromVarRef _instrAllocResult
        Reg.MemRepTag ->
          unsupported
        Reg.MemRepTuple ->
          unsupported
        Reg.MemRepUnit ->
          unsupported
        Reg.MemRepUnpacked {} ->
          unsupported

    fromTailCall :: Reg.InstrCall -> [Instruction]
    fromTailCall Reg.InstrCall {..} =
      case _instrCallType of
        Reg.CallFun sym ->
          mkArgAssigns _instrCallArgs
            ++ [mkJump (LabelRef (getLabel info sym))]
        Reg.CallClosure {} ->
          unsupported

    fromCall :: Reg.InstrCall -> Sem r [Instruction]
    fromCall Reg.InstrCall {..} = do
      lab <- freshLabel
      return $
        case _instrCallType of
          Reg.CallFun sym ->
            mkPushVars _instrCallLiveVars
              ++ mkArgAssigns _instrCallArgs
              ++ mkPush regSp (LabelRef lab)
              ++ [ mkJump (LabelRef (getLabel info sym)),
                   mkLabel lab,
                   mkMove (LRegRef (fromVarRef _instrCallResult)) (RegRef resultReg)
                 ]
              ++ mkPopVars _instrCallLiveVars
          Reg.CallClosure {} ->
            unsupported

    fromReturn :: Reg.InstrReturn -> [Instruction]
    fromReturn Reg.InstrReturn {..} =
      instrs
        ++ [mkMove (LRegRef resultReg) val]
        ++ mkPop regSp (LRegRef tmpReg)
        ++ [mkJump (RegRef tmpReg)]
      where
        (instrs, val) = fromValue _instrReturnValue

    fromBranch :: Reg.InstrBranch -> Sem r [Instruction]
    fromBranch Reg.InstrBranch {..} = do
      br1 <- fromRegCode info funInfo _instrBranchTrue
      br2 <- fromRegCode info funInfo _instrBranchFalse
      lab <- freshLabel
      lab' <- freshLabel
      return $
        instrs
          ++ [ mkJumpOnZero val (LabelRef lab)
             ]
          ++ br1
          ++ [mkJump (LabelRef lab'), mkLabel lab]
          ++ br2
          ++ [mkLabel lab']
      where
        (instrs, val) = fromValue _instrBranchValue

    fromCase :: Reg.InstrCase -> Sem r [Instruction]
    fromCase Reg.InstrCase {..} = do
      labs <- replicateM (length _instrCaseBranches) freshLabel
      lab' <- freshLabel
      brs <- mapM fromCaseBranch _instrCaseBranches
      def <- case _instrCaseDefault of
        Nothing -> return []
        Just code -> fromRegCode info funInfo code
      let jumps = zipWithExact (mkBranchJump val) labs _instrCaseBranches
      let brs' = zipWithExact (mkBranch lab') labs brs
      case _instrCaseIndRep of
        Reg.IndRepStandard ->
          return $ instrs ++ concat jumps ++ def ++ concat brs' ++ [mkLabel lab']
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
      where
        (instrs, val) = fromValue _instrCaseValue

    fromCaseBranch :: Reg.CaseBranch -> Sem r [Instruction]
    fromCaseBranch Reg.CaseBranch {..} = fromRegCode info funInfo _caseBranchCode

    mkBranchJump :: Value -> Text -> Reg.CaseBranch -> [Instruction]
    mkBranchJump val lab Reg.CaseBranch {..} =
      [ mkBinop OpIntEq (LRegRef (tmpReg + 1)) val (Const (getUID info _caseBranchTag)),
        mkBinop OpIntSub (LRegRef (tmpReg + 1)) (Const 1) (RegRef (tmpReg + 1)),
        mkJumpOnZero (RegRef (tmpReg + 1)) (LabelRef lab)
      ]

    mkBranch :: Text -> Text -> [Instruction] -> [Instruction]
    mkBranch lab' lab body = mkLabel lab : body ++ [mkJump (LabelRef lab')]
